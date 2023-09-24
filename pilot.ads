--                              -*- Mode: Ada -*-
-- Filename        : pilot.ads
-- Description     : Model a pilot who can fly.
-- Author          : Brian May
-- Created On      : Sat May  4 17:33:59 2002
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

package Pilots is

   type Pilot_Equipment is
      record
         DME, GNSS, INS, ILS : Boolean := False;
         RNP, UHF, ADF, VOR : Boolean := False;
         HF, DLink, TAC, VHF : Boolean := False;
         GPS, ADS : Boolean := False;
      end record;

   -- private object
   type Pilot is
      record
         Can_Fly_At_Night : Boolean;
         Can_Fly_In_Cloud : Boolean;
         Rated_To_Use : Pilot_Equipment;
      end record;

end Pilots;

