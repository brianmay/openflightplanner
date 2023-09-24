--                              -*- Mode: Ada -*-
-- Filename        : waypoints.ads
-- Description     : Waypoints and airports.
-- Author          : Brian May
-- Created On      : Sat May  4 17:33:21 2002
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

with Maps;
use Maps;

with Units;
use Units;

package Waypoints is

   No_Such_Waypoint : exception;
   Not_Connected : exception;

   type Waypoint is tagged
      record
         Pos : Position;
         Name, Abbrev : Unbounded_String;
      end record;
   function Get_Waypoint(Name : string) return Waypoint;

   type Airport is new Waypoint with
      record
         Height : Alt_Feet;
      end record;
   function Get_Waypoint(Name : string) return Airport;

   procedure Open(Filename : string);
   procedure Close;

end Waypoints;

