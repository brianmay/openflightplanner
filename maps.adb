--                              -*- Mode: Ada -*-
-- Filename        : maps.adb
-- Description     : Model the map; calculate distances between points; etc.
-- Author          : Brian May
-- Created On      : Sat May  4 17:29:04 2002
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
with Ada.Numerics;
use Ada.Numerics;

with Ada.Numerics.Generic_Elementary_Functions;

package body Maps is
   type F is Digits 15;

   package Maths is new Generic_Elementary_Functions(F);
   use Maths;

   function To_Radians(Value : F) return F is
   begin
     return F(Value)*F(Pi/180);
   end To_Radians;

   function To_Degrees(Value : F) return F is
   begin
     return Value*F(180.0/Pi);
   end To_Degrees;

   function Mod_Op(Value : F; M : F) return F is
     V : F := Value;
   begin
      while V > M loop
        V := V - M;
      end loop;
      while V < 0.0 loop
        V := V + M;
      end loop;
      return V;
   end Mod_Op;

   function Square(Value : F) return F is
   begin
        return Value**2;
   end Square;

   function To_Magnetic(The_Map : in Map;
                        Pos : in Position;
                        True_direction : in Degrees) return Degrees is
   begin
      -- FIXME: to_magnetic
      return(True_Direction - 11);
   end To_Magnetic;

   function To_True(The_Map : in Map;
                    Pos : in Position;
                    Mag_direction : in Degrees) return Degrees is
   begin
      -- FIXME: to_true
      return(Mag_Direction + 11);
   end To_True;

   function Get_Distance(The_Map : in Map;
                         A, B : in Position) return Vector is
      V : Vector;
      A_Lat : F := To_Radians(F(A.Latitude));
      A_Lon : F := -To_Radians(F(A.Longitude));
      B_Lat : F := To_Radians(F(B.Latitude));
      B_Lon : F := -To_Radians(F(B.Longitude));

      DLONW : F := Mod_Op(B_Lon - A_Lon, 2.0*Pi);
      DLONE : F := Mod_Op(A_Lon - B_Lon, 2.0*Pi);

      DPHI, Q, TC, DIST : F;

   begin
      if A_Lat = B_Lat then
        DPHI := Log(tan((B_Lat+0.0001)/2.0+pi/4.0)/tan(A_Lat/2.0+pi/4.0),2.7182818);
      else
        DPHI := Log(tan(B_Lat/2.0+Pi/4.0)/tan(A_Lat/2.0+pi/4.0),2.7182818);
      end if;

      if abs(B_Lat - A_Lat) < sqrt(1.5e-10) then
        Q := cos(A_Lat);
      else
        Q := (B_Lat - A_Lat)/DPHI;
      end if;

      if DLONW < DLONE then
        TC := Mod_Op(arctan(-DLONW/DPHI),2.0*pi);
        DIST := sqrt(Square(Q)*Square(DLONW)+Square(B_Lat-A_Lat));
      else
        TC := Mod_Op(arctan(DLONE/DPHI),2.0*pi);
        DIST := sqrt(Square(Q)*Square(DLONE)+Square(B_Lat-A_Lat));
      end if;
      TC := To_Degrees(TC);

      if A.Latitude < 0.0 and B.Latitude < 0.0 and B.Latitude < A.Latitude then
        if TC < 180.0 then
                TC := TC + 180.0;
        else
                TC := TC - 180.0;
        end if;
      end if;

      V := ( Distance => N_Miles(To_Degrees(DIST)*60.0),
             True_Direction=> Degrees(TC),
             Mag_Direction=>To_Magnetic(The_Map,A,Degrees(TC))
           );
      return V;
   end Get_Distance;

end Maps;
