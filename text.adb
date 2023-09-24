--                              -*- Mode: Ada -*-
-- Filename        : text.ads
-- Description     : text interface
-- Author          : Brian May
-- Created On      : Wed May  8 11:25:47 2002
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
with Ada.Text_Io;
use Ada.Text_Io;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

With Aircrafts;
use Aircrafts;

with Wabs;
use Wabs;
use Wabs.Result_Lists;

with Formatted_Output;
with Formatted_Output.Integer_Output;
with Formatted_Output.Modular_Output;
with Formatted_Output.Enumeration_Output;
use Formatted_Output;

with Units;
use Units;

with Winds;
use Winds;

with Flights;
use Flights.Leg_Lists;

with Flightplans;
use Flightplans.Flight_Lists;

package body Text is
   package SSR_Output is new Enumeration_Output(SSR_Type);
   use SSR_Output;

   package Alt_Feet_Output is new Integer_Output(Alt_Feet);
   use Alt_Feet_Output;

   package Knots_Output is new Integer_Output(Knots);
   use Knots_Output;

   package N_Miles_Output is new Integer_Output(N_Miles);
   use N_Miles_Output;

   package Minutes_Output is new Integer_Output(Minutes);
   use Minutes_Output;

   package Kgs_Output is new Integer_Output(Kgs);
   use Kgs_Output;

   package Moment_Output is new Integer_Output(Moment);
   use Moment_Output;

   package Litres_Output is new Integer_Output(Litres);
   use Litres_Output;

   package Degrees_Output is new Modular_Output(Degrees);
   use Degrees_Output;

   package Cog_Entry_Status_Output is new Enumeration_Output(Cog_Entry_Status);
   use Cog_Entry_Status_Output;

   procedure Print (The_Wab : in Wab) is
      The_Status : Cog_Entry_Status := Get_Status(The_Wab);
      I : Result_Iter := Get_Result_Iter(The_Wab);
   begin
      Put_Line("========== WAB -----------------");

      Put(+"%20s " & "Weight");
      Put(+"%4s " & "Kgs");
      Put(+"%6s " & "Arm");
      Put(+"%7s " & "Moment");
      Put(+"%s " & "Status");
      New_Line;

      while Has_Element(I) loop
         declare
            V : Result := Element(I);
            The_Moment : Moment := 0;
         begin
            -- calculate moment=weight*arm
            The_Moment := Moment(V.The_Weight) * Moment(V.The_Arm);

            -- debug
            Put(+"%20s "&To_String(V.Description));
            Put(+"%4d"&V.The_Weight);
            Put(+"%6s "&Arm'Image(V.The_Arm));
            Put(+"%7d "&V.The_Moment);
            Put(+"%c"&V.The_Status);
            New_Line;

            Next(I);
         end;
      end loop;

      Put(+"%c"&The_Status);
      Put_Line("");
   end Print;

   procedure Print (The_Equipment : in Equipment) is
   begin
      if The_Equipment.DME then
         Put("DME,");
      end if;
      if The_Equipment.GNSS then
         Put("GNSS,");
      end if;
      if The_Equipment.INS then
         Put("INS,");
      end if;
      if The_Equipment.ILS then
         Put("ILS,");
      end if;
      if The_Equipment.RNP then
         Put("RNP,");
      end if;
      if The_Equipment.UHF then
         Put("UHF,");
      end if;
      if The_Equipment.ADF then
         Put("ADF,");
      end if;
      if The_Equipment.VOR then
         Put("VOR,");
      end if;
      if The_Equipment.HF then
         Put("HF,");
      end if;
      if The_Equipment.DLink then
         Put("DLink,");
      end if;
      if The_Equipment.TAC then
         Put("TAC,");
      end if;
      if The_Equipment.VHF then
         Put("VHF,");
      end if;
      if The_Equipment.GPS then
         Put("GPS,");
      end if;
      if The_Equipment.ADS then
         Put("ADS,");
      end if;
      Put("SSR(");
      Put(+"%c"&The_Equipment.SSR);
      Put(")");
   end print;


   procedure Print (The_Aircraft : in Aircraft) is
      Data : constant Aircraft_Data := Get_Aircraft_Data(The_Aircraft);
      State : constant Aircraft_State := Get_Aircraft_State(The_Aircraft);
      The_Wab    : Wab;
   begin
      for I in Data.Fuel_Tank_List'Range loop
         declare
            Ft  : constant Fuel_Tank := Data.Fuel_Tank_List(I);
--            Ftr : constant Fuel_Tank_State := State.Fuel_Tank_List(I);
         begin
            Put("fuel_tank(");
            Put(To_String(Ft.Description));
            Put(")");
            New_Line;
         end;
      end loop;


      for I in Data.Seat_List'Range loop
         declare
            S  : constant Seat := Data.Seat_List(I);
--            Sr : constant Seat_State := State.Seat_List(I);
         begin
            Put("seat(");
            Put(To_String(S.Description));
            Put(")");
            New_Line;
         end;
      end loop;

--      may not be configured yet
--      The_Wab := Get_Wab(The_aircraft);
--      Print(The_Wab);
   end Print;

   procedure Print(The_Leg : in Leg) is
   begin
      Put(+"%35s " & To_String(Get_Destination(The_Leg).Name));
      Put(+"%5d " & Get_Lowest_Safe_Alt(The_Leg));
      Put(+"%4d " & Get_Alt(The_Leg));
      Put(+"%4d " & Get_True_Air_Speed(The_Leg));
      Put(+"%3d " & Get_Track(The_Leg));
      Put(+"%6s " &
          (
           To_String(+"%d/"&Get_Wind_Dir(The_Leg)) &
           To_string(+"%d"&Get_Wind_Speed(The_Leg)) 
          )
         );
      Put(+"%3d " & Get_Heading(The_Leg));
      Put(+"%3d " & Get_Ground_Speed(The_Leg));
      Put(+"%4d " & Get_Distance(The_Leg));
      Put(+"%3d " & Get_Eti(The_Leg));
      New_Line;
--         Put_Line("LEG("
--                  &"fuel="&Litres'Image(The_Leg.Fuel_Used)&","
--                  &"eti="&Minutes'Image(The_Leg.Eti)
--                  &")");
   end Print;

   procedure Print(The_Flight : in Flight) is
      I : Leg_Iter := Get_Leg_Iter(The_Flight);
      The_Wab : Wab;
   begin
      Put(+"%35s " & "Point");
      Put(+"%5s " & "LSALT");
      Put(+"%4s " & "ALT");
      Put(+"%4s " & "TAS");
      Put(+"%3s " & "TR");
      Put(+"%6s " & "WIND");
      Put(+"%3s " & "HDG");
      Put(+"%3s " & "GS");
      Put(+"%4s " & "DIST");
      Put(+"%3s " & "ETI");
      New_Line;

      Put(+"%35s " & To_String(Get_Takeoff(The_Flight).Name));
      New_Line;

      while Has_Element(I) loop
         Print(Element(I).all);
         Next(I);
      end loop;

      The_Wab := Get_Wab_Takeoff(The_Flight);
      Print(The_Wab);

      The_Wab := Get_Wab_Landing(The_Flight);
      Print(The_Wab);
   end Print;

   procedure Print(The_Flightplan : in Flightplan) is
      I : Flight_Iter := Get_Flight_Iter(The_Flightplan);
   begin
      while Has_Element(I) loop
         Print(Element(I).all);
         Next(I);
      end loop;


      Put(+"%12s"&"");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         Put(+" %3s"&"Min");
         Put(+" %3s"&"L");
         Next(I);
      end loop;
      New_Line;

      Put(+"%12s"&"Climb");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         declare
            The_Flight : Flight_Access := Element(I);
            Time : Time_Required_Record := Get_Time_Required(The_Flight.all);
            Fuel : Fuel_Required_Record := Get_Fuel_Required(The_Flight.all);
         begin
            Put(+" %3d"&Time.Climb);
            Put(+" %3d"&Fuel.Climb);
         end;
         Next(I);
      end loop;
      New_Line;

      Put(+"%12s"&"Descend");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         declare
            The_Flight : Flight_Access := Element(I);
            Time : Time_Required_Record := Get_Time_Required(The_Flight.all);
            Fuel : Fuel_Required_Record := Get_Fuel_Required(The_Flight.all);
         begin
            Put(+" %3d"&Time.Descend);
            Put(+" %3d"&Fuel.Descend);
         end;
         Next(I);
      end loop;
      New_Line;

      Put(+"%12s"&"Cruise");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         declare
            The_Flight : Flight_Access := Element(I);
            Time : Time_Required_Record := Get_Time_Required(The_Flight.all);
            Fuel : Fuel_Required_Record := Get_Fuel_Required(The_Flight.all);
         begin
            Put(+" %3d"&Time.Cruise);
            Put(+" %3d"&Fuel.Cruise);
         end;
         Next(I);
      end loop;
      New_Line;

      Put(+"%12s"&"Alternate");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         declare
            The_Flight : Flight_Access := Element(I);
            Time : Time_Required_Record := Get_Time_Required(The_Flight.all);
            Fuel : Fuel_Required_Record := Get_Fuel_Required(The_Flight.all);
         begin
            Put(+" %3d"&Time.Alternate);
            Put(+" %3d"&Fuel.Alternate);
         end;
         Next(I);
      end loop;
      New_Line;

      Put(+"%12s"&"Subtotal");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         declare
            The_Flight : Flight_Access := Element(I);
            Time : Time_Required_Record := Get_Time_Required(The_Flight.all);
            Fuel : Fuel_Required_Record := Get_Fuel_Required(The_Flight.all);
         begin
            Put(+" %3d"&Time.Subtotal);
            Put(+" %3d"&Fuel.Subtotal);
         end;
         Next(I);
      end loop;
      New_Line;

      Put(+"%12s"&"Var_Reserve");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         declare
            The_Flight : Flight_Access := Element(I);
            Time : Time_Required_Record := Get_Time_Required(The_Flight.all);
            Fuel : Fuel_Required_Record := Get_Fuel_Required(The_Flight.all);
         begin
            Put(+" %3d"&Time.Var_Reserve);
            Put(+" %3d"&Fuel.Var_Reserve);
         end;
         Next(I);
      end loop;
      New_Line;

      Put(+"%12s"&"Fix_Reserve");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         declare
            The_Flight : Flight_Access := Element(I);
            Time : Time_Required_Record := Get_Time_Required(The_Flight.all);
            Fuel : Fuel_Required_Record := Get_Fuel_Required(The_Flight.all);
         begin
            Put(+" %3d"&Time.Fix_Reserve);
            Put(+" %3d"&Fuel.Fix_Reserve);
         end;
         Next(I);
      end loop;
      New_Line;

      Put(+"%12s"&"Holding");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         declare
            The_Flight : Flight_Access := Element(I);
            Time : Time_Required_Record := Get_Time_Required(The_Flight.all);
            Fuel : Fuel_Required_Record := Get_Fuel_Required(The_Flight.all);
         begin
            Put(+" %3d"&Time.Holding);
            Put(+" %3d"&Fuel.Holding);
         end;
         Next(I);
      end loop;
      New_Line;

      Put(+"%12s"&"Taxi");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         declare
            The_Flight : Flight_Access := Element(I);
            Time : Time_Required_Record := Get_Time_Required(The_Flight.all);
            Fuel : Fuel_Required_Record := Get_Fuel_Required(The_Flight.all);
         begin
            Put(+" %3d"&Time.Taxi);
            Put(+" %3d"&Fuel.Taxi);
         end;
         Next(I);
      end loop;
      New_Line;

      Put(+"%12s"&"Awk");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         declare
            The_Flight : Flight_Access := Element(I);
            Time : Time_Required_Record := Get_Time_Required(The_Flight.all);
            Fuel : Fuel_Required_Record := Get_Fuel_Required(The_Flight.all);
         begin
            Put(+" %3d"&Time.Awk);
            Put(+" %3d"&Fuel.Awk);
         end;
         Next(I);
      end loop;
      New_Line;

      Put(+"%12s"&"Required");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         declare
            The_Flight : Flight_Access := Element(I);
            Time : Time_Required_Record := Get_Time_Required(The_Flight.all);
            Fuel : Fuel_Required_Record := Get_Fuel_Required(The_Flight.all);
         begin
            Put(+" %3d"&Time.Total);
            Put(+" %3d"&Fuel.Total);
         end;
         Next(I);
      end loop;
      New_Line;

      Put(+"%12s"&"Margin");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         declare
            The_Flight : Flight_Access := Element(I);
            Time : Time_Required_Record := Get_Time_Required(The_Flight.all);
            Fuel : Fuel_Required_Record := Get_Fuel_Required(The_Flight.all);
         begin
            Put(+" %3d"&Time.Margin);
            Put(+" %3d"&Fuel.Margin);
         end;
         Next(I);
      end loop;
      New_Line;

      Put(+"%12s"&"Endurance");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         declare
            The_Flight : Flight_Access := Element(I);
            Time : Time_Required_Record := Get_Time_Required(The_Flight.all);
            Fuel : Fuel_Required_Record := Get_Fuel_Required(The_Flight.all);
         begin
            Put(+" %3d"&Time.Endurance);
            Put(+" %3d"&Fuel.Endurance);
         end;
         Next(I);
      end loop;
      New_Line;

      Put(+"%12s"&"E Next Leg");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         declare
            The_Flight : Flight_Access := Element(I);
            Time : Time_Required_Record := Get_Time_Required(The_Flight.all);
            Fuel : Fuel_Required_Record := Get_Fuel_Required(The_Flight.all);
         begin
            Put(+" %3d"&Time.Endurance_Next_Leg);
            Put(+" %3d"&Fuel.Endurance_Next_Leg);
         end;
         Next(I);
      end loop;
      New_Line;

      Put(+"%12s"&"From");
      I := Get_Flight_Iter(The_Flightplan);
      while Has_Element(I) loop
         Put(+" %7s"&To_String(Get_Takeoff(Element(I).all).Abbrev));
         Next(I);
      end loop;
      New_Line;
   end Print;

end Text;
