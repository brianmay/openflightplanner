--                              -*- Mode: Ada -*-
-- Filename        : waypoints.adb
-- Description     : Waypoints and airports.
-- Author          : Brian May
-- Created On      : 2003-9-30 17:00
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
with Ada.Exceptions;
use Ada.Exceptions;

with GNU.DB.SQLite3;
use GNU.DB.SQLite3;

use GNU.DB.SQLite3.String_Vectors;

package body Waypoints is

   dBase : aliased Object;
   count : Natural := 0;

   procedure Check_Connected is
   begin
        if count = 0 then
                raise Not_Connected;
        end if;
  end Check_Connected;

   procedure Check_Error(rc : Return_Value) is
   begin
        if rc /= SQLITE_OK then
                Raise_Exception(Not_Connected'Identity,
                        "sqlite error ("&Return_Value'Image(rc)&"): "&Errmsg(dBase'access));
        end if;
   end Check_Error;

   function Get_Waypoint(Name : string) return Waypoint is
        rc : Return_Value;
        stmt : aliased Statement;
        rstmt : Statement_Reference := stmt'Unrestricted_Access;
        r : Waypoint;

   begin
        Check_Connected;
        rc := Prepare(dBase'Access, "SELECT Name, Latitude, Longitude FROM waypoints WHERE ID='"&Name&"'",rstmt);
        Check_Error(rc);

        -- broken
        -- rc := bind_text(rstmt, 1, "YLIL");
        -- Check_Error(rc);

        rc := Step(rstmt);
        if rc = SQLITE_ROW then
                r :=  (Pos => (Longitude =>  Longitude_Type'Value(column_text(rstmt,2)),
                                 Latitude => Latitude_Type'Value(column_text(rstmt,1))),
                               Name => To_Unbounded_String(column_text(rstmt,0)),
                               Abbrev => To_Unbounded_String(Name)
                              );

        elsif rc = SQLITE_DONE then
                Raise_Exception(No_Such_Waypoint'Identity,
                        "Cannot find waypoint "&Name);

        else
                Raise_Exception(Not_Connected'Identity,
                        "sqlite error ("&Return_Value'Image(rc)&"): "&Errmsg(dBase'access));
        end if;

        rc := finalize(rstmt);
        Check_Error(rc);
        return r;
   exception
   when Others =>
        rc := finalize(rstmt);
        raise;
   end;

   function Get_Waypoint(Name : string) return Airport is
        rc : Return_Value;
        stmt : aliased Statement;
        rstmt : Statement_Reference := stmt'Unrestricted_Access;
        r : Airport;

   begin
        Check_Connected;
        rc := Prepare(dBase'Access, "SELECT Name, Latitude, Longitude, Elevation FROM waypoints WHERE ID='"&Name&"' AND Elevation is not NULL",rstmt);
        Check_Error(rc);

        -- broken
        -- rc := bind_text(rstmt, 1, "YLIL");
        -- Check_Error(rc);

        rc := Step(rstmt);
        if rc = SQLITE_ROW then
                r :=  (Pos => (Longitude =>  Longitude_Type'Value(column_text(rstmt,2)),
                                 Latitude => Latitude_Type'Value(column_text(rstmt,1))),
                               Name => To_Unbounded_String(column_text(rstmt,0)),
                               Abbrev => To_Unbounded_String(Name),
                               Height => Alt_Feet'Value(column_text(rstmt,3))
                              );

        elsif rc = SQLITE_DONE then
                Raise_Exception(No_Such_Waypoint'Identity,
                        "Cannot find waypoint "&Name);

        else
                Raise_Exception(Not_Connected'Identity,
                        "sqlite error ("&Return_Value'Image(rc)&"): "&Errmsg(dBase'access));
        end if;

        rc := finalize(rstmt);
        Check_Error(rc);
        return r;
   exception
   when Others =>
        rc := finalize(rstmt);
        raise;
   end;

   procedure Open(Filename : string) is
        rc : Return_Value;
   begin
        if Count = 0 then
                rc := Open(dBase'access, Filename);
                Check_Error(rc);
        end if;
        Count := Count + 1;
   end Open;

   procedure Close is
        rc : Return_Value;
   begin
        Check_Connected;
        Count := Count - 1;
        if Count = 0 then
                rc := Close(dBase'access);
                Check_Error(rc);
        end if;
   end Close;

end Waypoints;
