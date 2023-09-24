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
package body Endurance is
   function Get_Required(Legs : in Leg_Vector;
                         Alternate_Leg : in Leg_Access;
                         Var_Reserve_Percent : in Percent;
                         Fix_Reserve :in Units;
                         Endurance : in Units)
                        return Required_Record is

      subtype F is Float;
      U : Used_Record;
      Fuel_Used : array (Flight_Phase) of Units := (others => 0);
      Subtotal, Var_Reserve : Units := 0;
      Alternate : Units := 0;
      Total : Units := 0;
   begin
      for I in Legs'Range loop
         declare
            L : Leg renames Legs(I).all;
         begin
            U := Get_Used(L);

            for Phase in Flight_Phase'range loop
               Fuel_Used(Phase) := Fuel_Used(Phase) + U(Phase);
            end loop;
         end;
      end loop;

         -- add alternate fuel
      if Alternate_Leg /= null then
         for Phase in Flight_Phase'Range loop
            U := Get_Used(Alternate_Leg.all);
            Alternate := Alternate + U(Phase);
         end loop;
      end if;

      Subtotal := Fuel_Used(Climb) + Fuel_Used(Cruise) + Fuel_Used(Descend)
        + Alternate;
      Var_Reserve := Units(F(Subtotal) * F(Var_Reserve_Percent)/100.0);
      -- Fix_Reserve is a parameter

      Total := Subtotal + Var_Reserve + Fix_Reserve
        + Fuel_Used(Taxi) + Fuel_Used(Awk) + Fuel_Used(Holding);

      if Total >= Endurance then
      	raise Not_Enough_Endurance;
      end if;

      return (
              Climb => Fuel_Used(Climb),
              Cruise => Fuel_Used(Cruise),
              Descend => Fuel_Used(Descend),
              Alternate => Alternate,
              Subtotal => Subtotal,
              Var_Reserve => Var_Reserve,
              Fix_Reserve => Fix_Reserve,
              Taxi => Fuel_Used(Taxi),
              Awk => Fuel_Used(Awk),
              Holding => Fuel_Used(Holding),
              Total => Total,
              Margin => Endurance - Total,
              Endurance => Endurance,
              Endurance_Next_Leg =>Endurance - Total + Fix_Reserve
             );
   end Get_Required;
end Endurance;
