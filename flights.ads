--                              -*- Mode: Ada -*-
-- Filename        : flights.ads
-- Description     : One flight in flightplan.
-- Author          : Brian May
-- Created On      : Sat May  4 17:24:09 2002
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

with Ada.Calendar;
use Ada.Calendar;

with Aircrafts;
use Aircrafts;

with Units;
use Units;

with Waypoints;
use Waypoints;

with Legs;
use Legs;

with Wabs;
use Wabs;

with Endurance;

with Ada.Containers.Vectors;

with Ada.Finalization;

with Ada.Calendar;
use Ada.Calendar;

package Flights is
   -- private object
   type Flight is tagged limited private;
   type Flight_Access is access Flight;

   package Time_Endurance is new Endurance(Minutes,
                                           Time_Used_Record,Get_Time_Used);
   package Fuel_Endurance is new Endurance(Litres,
                                           Fuel_Used_Record,Get_Fuel_Used);

   subtype Time_Required_Record is Time_Endurance.Required_Record;
   subtype Fuel_Required_Record is Fuel_Endurance.Required_Record;

   -- list of legs
   package Leg_Lists
      is new Ada.Containers.Vectors(
                        Element_Type=>Leg_Access,
                        Index_Type=>Positive);

   subtype Leg_Iter is Leg_Lists.Cursor;

   -- MUST CALL AFTER CREATING OBJECT
   procedure Set_Takeoff(The_Flight : in out Flight;
                         Departure_Time : in Time;
                         The_Airport : in Airport);

   -- Set_Takeoff_Change is optional
   procedure Set_Takeoff_Change(The_Flight : in out Flight;
                         Change : in Aircraft_Change);

   procedure Add_Leg(The_Flight : in out Flight;
                     The_Leg : in Leg_Access);

   procedure Set_Landing(The_Flight : in out Flight;
                         The_Airport : in Airport);

   procedure Set_Alternate_Leg(The_Flight : in out Flight;
                               The_Leg : in Leg_Access;
                               Alternate : in Airport);


   -- GET FIXED DATA
   function Get_Departure_Time(The_Flight : in Flight) return Time;
   function Get_Takeoff(The_Flight : in Flight) return Airport;
   function Get_Takeoff_Change(The_Flight : in Flight) return Aircraft_Change;
   function Get_Leg_Iter(The_Flight : in Flight) return Leg_Iter;
   function Get_Landing(The_Flight : in Flight) return Airport;
   function Get_Alternate(The_Flight : in Flight) return Airport;
   function Get_Alternate_Leg(The_Flight : in Flight) return Leg;


   -- CALLED BY FLIGHTS
   procedure Set_Aircraft(The_Flight : in out Flight;
                          The_Aircraft : in Aircraft_Access);

   -- Update this flight and the aircraft state and fuel
   Procedure Update(The_Flight : in out Flight);

   -- GET DATA AFTER UPDATE
   function Get_Time_Required(The_Flight : in Flight)
                             return Time_Required_Record;

   function Get_Fuel_Required(The_Flight : in Flight)
                             return Fuel_Required_Record;

   function Get_Wab_Takeoff(The_Flight : in Flight) return Wab;
   function Get_Wab_Landing(The_Flight : in Flight) return Wab;

private

   -- FIXME dont hardcode 15% and 45 minutes reserves
   Var_Reserve_Percent : constant := 15;
   Fix_Reserve_Minutes : constant Minutes := 45;

   type Access_Aircraft_Change is access Aircraft_Change;

   type Flight is new Ada.Finalization.Limited_Controlled with
      record
         Departure_Time : Time;

         Takeoff : Airport;
         Landing : Airport;
         Legs : Leg_Lists.Vector;
         The_Aircraft : Aircraft_Access := null;

         Alternate : Airport;
         Alternate_Leg : Leg_Access;

         Endurance_Time : Minutes;
         Endurance_Fuel : Litres;

         Time_Required : Time_Required_Record;
         Fuel_Required : Fuel_Required_Record;

         Wab_takeoff, Wab_Landing : Wab;

         Takeoff_Change : Access_Aircraft_Change;
      end record;

      procedure Initialize(Object : in out Flight);
      procedure Adjust    (Object : in out Flight);
      procedure Finalize  (Object : in out Flight);
end Flights;
