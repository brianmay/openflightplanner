--                              -*- Mode: Ada -*-
-- Filename        : flightplans.adb
-- Description     : A flightplan.
-- Author          : Brian May
-- Created On      : Sat May  4 17:24:49 2002
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

package body Flightplans is
   use Flight_Lists;

   procedure Free is
     new Unchecked_Deallocation(Flight,Flight_Access);

   procedure Add_Flight(The_Flightplan : in out Flightplan;
                        The_Flight : in Flight_Access) is
   begin
      Append(The_Flightplan.Flights,The_Flight);
   end Add_Flight;

   Procedure Update(The_Flightplan : in out Flightplan) is
      I : Flight_Iter := First(The_Flightplan.Flights);
   begin
      while Has_Element(I) loop
         declare
            F : constant Flight_Access := Element(I);
         begin
            Set_Aircraft(F.all,The_Flightplan.The_Aircraft);
            Update(F.all);
         end;
         Next(I);
      end loop;
   end Update;

   procedure Set_Sar_Data(The_Flightplan : in out Flightplan;
                     Sar : in Sar_Data) is
   begin
      The_Flightplan.Sar := Sar;
   end Set_Sar_Data;

   procedure Set_Aircraft(The_Flightplan : in out Flightplan;
                          The_Aircraft : in Aircraft_Access) is
   begin
      The_Flightplan.The_Aircraft := The_Aircraft;
   end Set_Aircraft;

   function Get_Sar_Data(The_Flightplan : in Flightplan) Return Sar_Data is
   begin
      return The_Flightplan.Sar;
   end Get_Sar_Data;

   function Get_Flight_Iter(The_Flightplan : in Flightplan) return Flight_Iter is
   begin
      return First(The_Flightplan.Flights);
   end Get_Flight_Iter;

   procedure Initialize(Object : in out Flightplan) is
   begin
      null;
   end Initialize;

   procedure Adjust    (Object : in out Flightplan) is
   begin
      null;
   end Adjust;

   procedure Finalize  (Object : in out FlightPlan) is
      I : Flight_Iter := First(Object.Flights);
   begin
      while Has_Element(I) loop
         declare
            L : Flight_Access := Element(I);
         begin
            Free(L);
         end;
         Next(I);
      end loop;
   end Finalize;

end Flightplans;
