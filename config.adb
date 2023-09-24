--                              -*- Mode: Ada -*-
-- Filename        : config.adb
-- Description     : configuration management
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
with Waypoints;

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Exceptions;
use  Ada.Exceptions;

package body Config is
   Waypoints_Database : constant String := "waypoints.db";

   procedure Initialize is
   begin
        Waypoints.open(Waypoints_Database);
   exception
   when E : Waypoints.Not_Connected =>
        Put_Line(Standard_Error,"Waypoints connection Failed!");
        Put_Line(Standard_Error,Exception_Message(E));
        raise;
   end Initialize;

   procedure Finalize is
   begin
        Waypoints.close;
   exception
   when E : Waypoints.Not_Connected =>
        Put_Line(Standard_Error,"Waypoints disconnection failed!");
        Put_Line(Standard_Error,Exception_Message(E));
        raise;
   end Finalize;

end Config;
