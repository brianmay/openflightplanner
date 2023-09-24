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

with Aircrafts;
use Aircrafts;

with Legs;
use Legs;

generic
   type Units is range <>;
   type Used_Record is array (Flight_Phase) of Units;
   With function Get_Used(L : in Leg) return Used_Record;

package Endurance is
   type Required_Record is
      record
         Cruise, Climb, Descend, Alternate : Units := 0;
         Subtotal : Units := 0;

         Var_Reserve : Units := 0;
         Fix_Reserve : Units := 0;

         Holding, Taxi, Awk : Units := 0;
         Total : Units := 0;

         Margin, Endurance : Units := 0;

         Endurance_Next_Leg : Units := 0;
      end record;

   Not_Enough_Endurance : Exception;

   function Get_Required(Legs : in Leg_Vector;
                         Alternate_Leg : in Leg_Access;
                         Var_Reserve_Percent : in Percent;
                         Fix_Reserve : in  Units;
                         Endurance : in Units)
                        return Required_Record;
end Endurance;
