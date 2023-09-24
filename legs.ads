--                              -*- Mode: Ada -*-
-- Filename        : legs.ads
-- Description     : One leg in flight in flightplan
-- Author          : Brian May
-- Created On      : Sat May  4 17:22:32 2002
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

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Units;
use Units;

with Aircrafts;
use Aircrafts;

with Waypoints;
use Waypoints;

with Winds;
use Winds;

package legs is
   -- private object
   type Leg is tagged private;
   type Leg_Access is access all Leg;
   type Leg_Vector is array (Integer range <>) of Leg_Access;

   type Time_Used_Record is new Time_Record;
   type Fuel_Used_Record is new Fuel_Record;

   -- MUST CALL AFTER CREATING OBJECT
   procedure Initialize(The_Leg : out Leg;
                        Lowest_Safe_Alt, Alt : in Alt_Feet;
                        Track : in Degrees;
                        Distance : in N_Miles;
                        Awk_Time : in Minutes;
                        Awk_Rmk : in Unbounded_String;
                        Holding_Time : in Minutes;
                        Taxi_Time : in Minutes);

   -- CALL ANY TIME
   procedure Set_Wind(The_Leg : in out Leg;
                      The_Wind : in Wind );

   -- REQUIRED IF NOT LAST LEG; OTHERWISE CALLED BY FLIGHTS
   -- if last leg is no longer last leg, need to call these functions.
   procedure Set_Destination(The_Leg : in out Leg;
                             Destination : in Waypoint);
   procedure Set_Alt_End(The_Leg : in out Leg;
                         Alt : in Alt_Feet);


   -- CALLED BY FLIGHTS
   procedure Set_Alt_Start(The_Leg : in out Leg;
                           Alt : in Alt_Feet);

   procedure Set_Aircraft(The_Leg : in out Leg;
                          The_Aircraft : in Aircraft_Access);

   procedure Update(The_Leg : in out Leg);

   -- get fixed data
   function Get_Destination(The_Leg : in Leg) Return Waypoint;
   function Get_Lowest_Safe_Alt(The_Leg : in Leg) return Alt_Feet;
   function Get_Alt(The_Leg : in Leg) return Alt_Feet;
   function Get_Alt_Start(The_Leg : in Leg) return Alt_Feet;
   function Get_Alt_End(The_Leg : in Leg) return Alt_Feet;
   function Get_Track(The_Leg : in Leg) return Degrees;
   function Get_Distance(The_Leg : in Leg) return N_Miles;
   function Get_Awk_Time(The_Leg : in Leg) return Minutes;
   function Get_Awk_Rmk(The_Leg : in Leg) return Unbounded_String;
   function Get_Holding_Time(The_Leg : in Leg) return Minutes;
   function Get_Wind_Speed(The_Leg : in Leg) return Knots;
   function Get_Wind_Dir(The_Leg : in Leg) return Degrees;

   -- get calculated values
   --
   -- To use these functions:
   -- 1. Initialize values (*before* step 2):
   --    - Set_Wind
   --    - Set_Aircraft, this sets:
   --       + True_Air_Speed
   --       + Fuel_Consumption
   --    - Set_Alt_Start()
   --    - Set_Alt_End()
   -- 2. Call Update().
   -- 3. Call required function.
   function Get_True_Air_Speed(The_Leg : in Leg) return Knots;
   function Get_Heading(The_Leg : in Leg) return Degrees;
   function Get_Ground_Speed(The_Leg : in Leg) return Knots;
   function Get_Eti(The_Leg : in Leg) return Minutes;
   function Get_Time_Used(The_Leg : in Leg) return Time_Used_Record;
   function Get_Fuel_Used(The_Leg : in Leg) return Fuel_Used_Record;

   Not_Uptodate : Exception;
   Not_Configured : exception;

private
   -- FIXME: don't hardcode climb_rate, descend_rate
   Climb_Rate   : constant := 1000.0;
   Descend_Rate : constant := 1000.0;
--   Taxi_Time    : constant Minutes := 5;

   type Leg is tagged
      record
         -- fixed data
         Destination    : Waypoint;
         Alt_Start, Alt_End : Alt_Feet;
         Lowest_Safe_Alt: Alt_Feet;
         Alt            : Alt_Feet;
         Track          : Degrees;
         Awk_Time       : Minutes;
         Awk_Rmk        : Unbounded_String;
         Holding_Time   : Minutes;
         Taxi_Time      : Minutes;

         -- determined from aircraft
         The_Aircraft   : Aircraft_Access;
         Air_Speed      : Air_Speed_Record;
         Fuel_Consumption : Fuel_Consumption_Record;
         True_Air_Speed  : Knots;

         --determined from wind
         The_Wind       : Wind;
         Heading        : Degrees;
         Ground_Speed   : Knots;
         Distance       : N_Miles;
         Eti            : Minutes;
         Time_Used      : Time_Used_Record;
         Fuel_Used      : Fuel_Used_Record;

         -- is wind calculations up-to-date?
         Uptodate       : Boolean := False;

         -- have required parameters been set?
         Configured_Aircraft  : Boolean :=  False;
      end record;

end Legs;
