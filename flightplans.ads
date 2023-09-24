--                              -*- Mode: Ada -*-
-- Filename        : flightplans.ads
-- Description     : A flightplan.
-- Author          : Brian May
-- Created On      : Sat May  4 17:24:29 2002
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

with Flights;
use Flights;

with Ada.Containers.Vectors;

with Ada.Finalization;

package Flightplans is
   -- private object
   type Flightplan is tagged limited private;
   type Flightplan_Access is access Flightplan;

   type SAR_Type is (None,Arrival,Departure);
   type Quantity is range 0..10;
   type SAR_Equipment is
      record
         Light, UHF, VHF, Floures : Boolean := False;
         First_Aid, Rations, Water : Boolean := False;
         Dinghies : Quantity := 0;
         Dinghies_Capacity : Quantity := 0;
         Dinghies_Colour : Unbounded_String;
         Dinghies_Covers : Boolean := False;
      end record;

   type SAR_Data is
      record
         What : SAR_Type;
         Latest : Time;
         Where : Unbounded_String;
         Phone : Unbounded_String;
         Fax   : Unbounded_String;
         Equipment : SAR_Equipment;
      end record;

   -- list of flights
   package Flight_Lists
      is new Ada.Containers.Vectors(
                        Element_Type=>Flight_Access,
                        Index_Type=>Positive);

   subtype Flight_Iter is Flight_Lists.Cursor;

   -- sub programs
   procedure Add_Flight(The_Flightplan : in out Flightplan;
                        The_Flight : in Flight_Access);

   procedure Set_SAR_Data(The_Flightplan : in out Flightplan;
                     SAR : in SAR_Data);

   procedure Set_Aircraft(The_Flightplan : in out Flightplan;
                          The_Aircraft : in Aircraft_Access);

   procedure Update(The_Flightplan : in out Flightplan);
   function Get_SAR_Data(The_Flightplan : in Flightplan) Return SAR_Data;

   function Get_Flight_Iter(The_Flightplan : in Flightplan) return Flight_Iter;

private
   type Flightplan is new Ada.Finalization.Limited_Controlled with
      record
         Flights : Flight_Lists.Vector;
         SAR : SAR_Data;
         The_Aircraft : Aircraft_Access;
      end record;

      procedure Initialize(Object : in out Flightplan);
      procedure Adjust    (Object : in out Flightplan);
      procedure Finalize  (Object : in out Flightplan);
end Flightplans;
