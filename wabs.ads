--                              -*- Mode: Ada -*-
-- Filename        : wabs.ads
-- Description     : Weight and balance calculator.
-- Author          : Brian May
-- Created On      : Sat May  4 17:31:03 2002
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

with Ada.Containers.Vectors;

package Wabs is
   -- basic weight information
   type Arm is delta 0.1 range 1_000.0 .. 5_000.0;
   type Kgs is range 0..5_000;
   type Moment is range 0..4_000_000;
   type Cog_Entry_Type is (Fuel,Passenger,Luggage,Bew,Total);
   type Cog_Entry_Status is (Ok, Too_Much, Too_Heavy, Outside_Limits);

   -- basic weight information
   type Weight_Limits is
      record
         Mtow      : Kgs := 0;
         Mlw       : Kgs := 0;
         Mzfw      : Kgs := 0;
      end record;

   -- centre of gravity entry
   type Cog_Entry is
      record
         Description : Unbounded_String;
         The_Weight  : Kgs := 0;
         Max_Weight  : Kgs := Kgs'Last;
         The_Arm     : Arm := Arm'First;
         The_Type    : Cog_Entry_Type := Fuel;
         The_Status  : Cog_Entry_Status := Ok;
      end record;

   -- results
   type Result is
      record
         Description : Unbounded_String;
         The_Weight  : Kgs := 0;
         The_Arm     : Arm := Arm'First;
         The_Moment  : Moment := Moment'First;
         The_Type    : Cog_Entry_Type := Fuel;
         The_Status  : Cog_Entry_Status := Ok;
      end record;

   -- centre of gravity limits entry
   type Cog_Limits_Entry is
      record
         The_Weight : Kgs := 0;
         The_Arm : Arm := Arm'First;
      end record;

   -- COG lists
   package Cog_Lists
      is new Ada.Containers.Vectors(
                        Element_Type=>Cog_Entry,
                        Index_Type=>Positive);

   -- COG limits lists
   package Cog_Limits_Lists
      is new Ada.Containers.Vectors(
                        Element_Type=>Cog_Limits_Entry,
                        Index_Type=>Positive);

   -- Result lists
   package Result_Lists
      is new Ada.Containers.Vectors(
                        Element_Type=>Result,
                        Index_Type=>Positive);

   subtype Result_Iter is Result_Lists.Cursor;

   -- private object
   type Wab is tagged private;
   type Wab_Access is access Wab;

   -- sub programs
   procedure Add_Cog_Entry(The_Wab : in out Wab;
                           The_Cog_Entry : in Cog_Entry);
   procedure Set_Cog_Limits(The_Wab : in out Wab;
                                  Cog_Limits : in Cog_Limits_Lists.Vector);
   procedure Set_Weight_Limits(The_Wab : in out Wab;
                               The_Weight_Limits : in Weight_Limits);

   procedure Calculate(The_Wab : in out Wab);

   function Get_Status(The_Wab : in Wab) return Cog_Entry_Status;


   function Get_Result_Iter(The_Wab : in Wab) return Result_Iter;

private
   -- The object
   type Wab is tagged
      record
         Cog_List : Cog_Lists.Vector;
         Results  : Result_Lists.Vector;
         Cog_Limits : Cog_Limits_Lists.Vector;
         The_Weight_Limits : Weight_Limits := (others => 0);
         The_Status : Cog_Entry_Status := Too_Heavy;
      end record;

end Wabs;
