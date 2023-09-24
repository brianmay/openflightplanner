--                              -*- Mode: Ada -*-
-- Filename        : flights.adb
-- Description     : One flight in flightplan.
-- Author          : Brian May
-- Created On      : Sat May  4 17:23:26 2002
-- Last Modified By: .
-- Last Modified On: .
-- Update Count    : 0
-- Status          : Unknown, Use with caution!
-----------------------------------------------------------------------
--          Flightplanner - Open source flightplanner                --
--                                                                   --
--                     Copyright (C) 2002-2004                       --
--                            Brian May                              --
--                                                                   --
-- This program is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This program is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this program; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------

with Waypoints;
use Waypoints;

with Winds;
use Winds;

with Unchecked_Deallocation;

with Ada.Finalization;

package body Flights is
   use Leg_Lists;

   procedure Free is
        new Unchecked_Deallocation(Leg,Leg_Access);
   procedure Free is
        new Unchecked_Deallocation(Aircraft_Change,Access_Aircraft_Change);

   procedure Update_Endurance(The_Flight : in out Flight) is
      In_I : Leg_Iter := First(The_Flight.Legs);

      I : constant Natural := First_Index(The_Flight.Legs);
      J : constant Natural := Last_Index(The_Flight.Legs);
      A : Leg_Vector(I..J);

      Fix_Reserve_Litres : Litres
        := Get_Fuel_Used(The_Flight.The_Aircraft.all,
                         Cruise, Fix_Reserve_Minutes);
   begin
      for Out_I in A'Range loop
         A(Out_I) := Element(In_I);
         Next(In_I);
      end loop;

      The_Flight.Time_Required
        := Time_Endurance.Get_Required(A,
                                       The_Flight.Alternate_Leg,
                                       Var_Reserve_Percent,
                                       Fix_Reserve_Minutes,
                                       The_Flight.Endurance_Time);
      The_Flight.Fuel_Required
        := Fuel_Endurance.Get_Required(A,
                                       The_Flight.Alternate_Leg,
                                       Var_Reserve_Percent,
                                       Fix_Reserve_Litres,
                                       The_Flight.Endurance_Fuel);

   end Update_Endurance;

   Procedure Update(The_Flight : in out Flight) is
      The_Aircraft : Aircraft renames The_Flight.The_Aircraft.all;
      I : Leg_Iter := First(The_Flight.Legs);
      Alt : Alt_Feet;
      Fuel_Start : Litres;
   begin
      if The_Flight.Takeoff_Change /= null then
         -- FIXME: need to ensure somewhere that state is always set
         -- FIXME: for first leg in the first flight
         Update_Aircraft_State(The_Aircraft, The_Flight.Takeoff_Change.all);
      end if;

      Fuel_Start := Get_Total_Fuel(The_Aircraft);

      The_Flight.Endurance_Time
        := Get_Time_Used(The_Aircraft,Cruise,Fuel_Start);
      The_Flight.Endurance_Fuel := Fuel_Start;

      The_Flight.Wab_Takeoff := Get_Wab(The_Aircraft);

      Alt := The_Flight.Takeoff.Height;
      while Has_Element(I) loop
         declare
            L : constant Leg_Access := Element(I);
         begin
            Set_Alt_Start(L.all,Alt);
            Set_Aircraft(L.all,The_Flight.The_Aircraft);
            Alt := Get_Alt_End(L.all);

            Next(I);

            if not Has_Element(I) then
               Set_Alt_End(L.all,The_Flight.Landing.Height);
               Set_Destination(L.all,Waypoint(The_Flight.Landing));
            end if;
            Update(L.all);
         end;
      end loop;

      if The_Flight.Alternate_Leg /= Null then
         declare
            L : constant Leg_Access := The_Flight.Alternate_Leg;
         begin
            Set_Alt_Start(L.all,Alt);
            Set_Alt_End(L.all,The_Flight.Alternate.Height);
            Set_Destination(L.all,Waypoint(The_Flight.Alternate));
            Set_Aircraft(L.all,The_Flight.The_Aircraft);
            Update(L.all);
         end;
      end if;

      Update_Endurance(The_Flight);

      declare
         Fuel_Used : Litres := Fuel_Start
           - The_Flight.Fuel_Required.Endurance_Next_Leg;
      begin
         Consume_Fuel(The_Aircraft,Fuel_Used);
      end;

      The_Flight.Wab_Landing := Get_Wab(The_Aircraft);
   end Update;


   procedure Set_Takeoff(The_Flight : in out Flight;
                         Departure_Time : in Time;
                         The_Airport : in Airport) is
   begin
      The_Flight.Departure_Time := Departure_Time;
      The_Flight.Takeoff := The_Airport;
   end;

   procedure Set_Takeoff_Change(The_Flight : in out Flight;
                               Change : in Aircraft_Change) is
   begin
      Free(The_Flight.Takeoff_Change);
      The_Flight.Takeoff_Change := new Aircraft_Change'(Change);
   end Set_Takeoff_Change;

   procedure Add_Leg(The_Flight : in out Flight;
                     The_Leg : in Leg_Access) is
   begin
      Append(The_Flight.Legs,The_Leg);
   end Add_Leg;

   procedure Set_Landing(The_Flight : in out Flight;
                         The_Airport : in Airport) is
   begin
      The_Flight.Landing := The_Airport;
   end;

   function Get_Departure_Time(The_Flight : in Flight) return Time is
   begin
      return The_Flight.Departure_Time;
   end Get_Departure_Time;

   function Get_Takeoff(The_Flight : in Flight) return Airport is
   begin
      return The_Flight.Takeoff;
   end Get_Takeoff;

   function Get_Takeoff_Change(The_Flight : in Flight)
         return Aircraft_Change is
   begin
      return The_Flight.Takeoff_Change.all;
   end Get_Takeoff_Change;

   function Get_Landing(The_Flight : in Flight) return Airport is
   begin
      return The_Flight.Landing;
   end Get_Landing;

   procedure Set_Alternate_Leg(The_Flight : in out Flight;
                               The_Leg : in Leg_Access;
                               Alternate : in Airport) is
   begin
      The_Flight.Alternate_Leg := The_Leg;
      The_Flight.Alternate := Alternate;
   end Set_Alternate_Leg;

   procedure Set_Aircraft(The_Flight : in out Flight;
                          The_Aircraft : in Aircraft_Access) is
   begin
      The_Flight.The_Aircraft := The_Aircraft;
   end Set_Aircraft;

   function Get_Time_Required(The_Flight : in Flight)
                             return Time_Required_Record is
   begin
      return The_Flight.Time_Required;
   end Get_Time_Required;

   function Get_Fuel_Required(The_Flight : in Flight)
                             return Fuel_Required_Record is
   begin
      return The_Flight.Fuel_Required;
   end Get_Fuel_Required;

   function Get_Wab_Takeoff(The_Flight : in Flight) return Wab is
   begin
      return The_Flight.Wab_Takeoff;
   end Get_Wab_Takeoff;

   function Get_Wab_Landing(The_Flight : in Flight) return Wab is
   begin
      return The_Flight.Wab_Landing;
   end Get_Wab_Landing;

   function Get_Leg_Iter(The_Flight : in Flight) return Leg_Iter is
   begin
      return First(The_Flight.Legs);
   end Get_Leg_Iter;

   function Get_Alternate(The_Flight : in Flight) return Airport is
   begin
      return The_Flight.Alternate;
   end Get_Alternate;

   function Get_Alternate_Leg(The_Flight : in Flight) return Leg is
   begin
      return The_Flight.Alternate_Leg.all;
   end Get_Alternate_Leg;

   procedure Initialize(Object : in out Flight) is
   begin
      null;
   end Initialize;

   procedure Adjust    (Object : in out Flight) is
   begin
      null;
   end Adjust;

   procedure Finalize  (Object : in out Flight) is
      I : Leg_Iter := First(Object.Legs);
   begin
      while Has_Element(I) loop
         declare
            L : Leg_Access := Element(I);
         begin
            Free(L);
         end;
         Next(I);
      end loop;
      Free(Object.Takeoff_Change);
   end Finalize;

end Flights;
