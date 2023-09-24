--                              -*- Mode: Ada -*-
-- Filename        : weather.adb
-- Description     : What is the weather?
-- Author          : Brian May
-- Created On      : Sat May  4 17:26:00 2002
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
package body Weather is

   function Get_Wind_At_Position_And_Height(Pos : in Position;
                                            Height : in Alt_Feet)
                                           return Wind is
      The_Wind : Wind;
   begin
      -- FIXME: get weather at position and height
      Initialize(The_Wind,Speed=>20,Direction=>160);
      return(The_Wind);
   end Get_Wind_At_Position_And_Height;
end Weather;
