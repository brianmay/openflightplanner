--                              -*- Mode: Ada -*-
-- Filename        : xml.ads
-- Description     : XML File Reading/Writing
-- Author          : Brian May
-- Created On      : Wed May 15 16:23:45 2002
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
with Aircrafts;
with Legs;
with Flights;
with Flightplans;

package Xml is
   Invalid_XML : exception;

   function Get_Equipment(Filename : string) return Aircrafts.Equipment;
   function Get_Aircraft_Data(Filename : string) return Aircrafts.Aircraft_Data;
   function Get_Leg(Filename : string) return Legs.Leg_Access;
   function Get_Flight(Filename : string) return Flights.Flight_Access;
   function Get_Flightplan(Filename : string) return Flightplans.Flightplan_Access;
end Xml;
