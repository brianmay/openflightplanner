--                              -*- Mode: Ada -*-
-- Filename        : winds.adb
-- Description     : Model the wind at a given location.
-- Author          : Brian May
-- Created On      : Sat May  4 17:27:00 2002
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
with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Numerics;
use Ada.Numerics;

with Ada.Numerics.Generic_Elementary_Functions;

package body Winds is
   type F is new Float;
   subtype Radians is F;

   package Maths is new Generic_Elementary_Functions(F);
   use Maths;

   procedure Initialize(The_Wind : in out Wind;
                        Direction : in Degrees;
                        Speed : in Knots) is
   begin
      The_Wind := ( Speed => Speed, Direction => Direction );
   end;


   function Get_Direction(The_Wind : in Wind) return Degrees is
   begin
      return The_Wind.Direction;
   end Get_Direction;

   function Get_Speed(The_Wind : in Wind) return Knots is
   begin
      return The_Wind.Speed;
   end Get_Speed;

   function To_Radians(Value : Degrees) return Radians is
   begin
     return Radians(F(Value)*F(Pi/180));
   end To_Radians;

   function To_Degrees(Value : Radians) return Degrees is
   begin
     return Degrees(Value*F(180.0/Pi));
   end To_Degrees;

   procedure Compensate_For_Wind(The_Wind : in Wind;
                                 Direction : in out Degrees;
				 Speed : in out Knots) is
      Cross_Wind, Tail_Wind : F;
      Old_Speed : F := F(Speed);
      New_Speed : F;
      New_Direction : Radians;
   begin
      --Put_Line("Desired: "&Degrees'Image(Direction)
      --        &"/"&Knots'Image(Speed));

      --Put_Line("Wind: "&Degrees'Image(The_Wind.Direction)
      --        &"/"&Knots'Image(The_Wind.Speed));

      Cross_Wind := F(The_Wind.Speed)
        * Sin(To_Radians(The_Wind.Direction - Direction));
      Tail_Wind := -F(The_Wind.Speed)
        * Cos(To_Radians(The_Wind.Direction - Direction));

      --Put_Line("Cross Wind: "&F'Image(Cross_Wind));
      --Put_Line("Tail Wind: "&F'Image(Tail_Wind));

      New_Direction := Arcsin(Cross_Wind/Old_Speed) + To_Radians(Direction);

      New_Speed := Cos(New_Direction - To_Radians(Direction))*Old_Speed+
        tail_wind;

      Speed := Knots(New_Speed);
      Direction := To_Degrees(New_Direction);

      --Put_Line("Result: "&Degrees'Image(Direction)
      --        &"/"&Knots'Image(Speed));

   end Compensate_For_Wind;

end Winds;
