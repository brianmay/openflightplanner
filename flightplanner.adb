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
with Xml;
use Xml;

with Aircrafts;
use Aircrafts;

with Flightplans;
use Flightplans;

with Ada.Command_Line;
use Ada.Command_Line;

with Text_IO;
use Text_IO;

with Text;
use Text;

with Config;
use Config;

with Unchecked_Deallocation;

procedure Flightplanner is
   The_Aircraft : Aircraft_Access;
   The_Flightplan : Flightplan_Access;

   procedure Free is
        new Unchecked_Deallocation(Aircraft,Aircraft_Access);

   procedure Free is
        new Unchecked_Deallocation(Flightplan,Flightplan_Access);

   Argument_Error : exception;

begin
   if Argument_Count <= 1 then
        Put_Line(Standard_Error,"Error; need two parameters");
        New_Line(Standard_Error);
        Put_Line(Standard_Error,"Usage: flightplanner <aircraft.xml> "&
                        "<flightplan.xml>");
        Set_Exit_Status(Failure);
        return;
   end if;

   Config.Initialize;

   declare
        Data : Aircraft_Data := Get_Aircraft_Data(Argument(1));
   begin
        The_Aircraft := new Aircraft(Data.Fuel_Tanks,Data.Seats);
        Set_Aircraft_Data(The_Aircraft.all, Data);
        Set_Aircraft_Empty_State(The_Aircraft.all);
   end;

   The_Flightplan := Get_Flightplan(Argument(2));
   Set_Aircraft(The_Flightplan.all,The_Aircraft);
   Update(The_Flightplan.all);
   Print(The_Flightplan.all);
--        Print(The_Aircraft.all);

   Config.Finalize;

   Free(The_Flightplan);
   Free(The_Aircraft);

   Set_Exit_Status(Success);
end Flightplanner;
