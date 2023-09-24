--                              -*- Mode: Ada -*-
-- Filename        : maps.ads
-- Description     : Model the map; calculate distances between points; etc.
-- Author          : Brian May
-- Created On      : Sat May  4 17:28:54 2002
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

with Units;
use Units;

package Maps is
   -- private object
   type Map is tagged private;

   type Longitude_Type is digits 15 range -180.0 .. 180.0;
   type Latitude_Type  is digits 15 range -90.0 .. 90.0;

   type Position is
      record
         Longitude : Longitude_Type;
         Latitude : Latitude_Type;
      end record;

   type Vector is
      record
         Distance : N_Miles;
         True_Direction : Degrees;
         Mag_Direction : Degrees;
      end record;

   function Get_Distance(The_Map : in Map;
                         A, B : in Position) return Vector;


   function To_Magnetic(The_Map : in Map;
                        Pos : in Position;
                        True_direction : in Degrees) return Degrees;

   function To_True(The_Map : in Map;
                    Pos : in Position;
                    Mag_direction : in Degrees) return Degrees;

private
   type Map is tagged
      record
         null;
      end record;

end Maps;

