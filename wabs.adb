--                              -*- Mode: Ada -*-
-- Filename        : wabs.adb
-- Description     : Weight and balance calculator.
-- Author          : Brian May
-- Created On      : Sat May  4 17:31:20 2002
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
package body Wabs is
   use Cog_Lists;
   use Cog_Limits_Lists;
   use Result_Lists;

   -- PRIVATE FUNCTIONS

   procedure Set_Cog_Entry_Status(Status : in out Cog_Entry_Status;
                                  New_Status : in Cog_Entry_Status) is
   begin
      if Status = Ok then
         Status := New_Status;
      end if;
   end Set_Cog_Entry_Status;

   function Is_Cog_Within_Limits(The_Wab : in Wab;
                                 The_Weight : in Kgs;
                                 The_Arm : in Arm) return Boolean is

      I : Cog_Limits_Lists.Cursor
      		:= First(The_Wab.Cog_Limits);
      Last_Entry : constant Cog_Limits_Entry := Last_Element(The_Wab.Cog_Limits);
      Weight_Prev : Kgs := Last_Entry.The_Weight;
      Arm_Prev    : Arm := Last_Entry.The_Arm;
      Weight_Max  : Kgs := Kgs'First;
      Weight_Min  : Kgs := Kgs'Last;
      Valid : Boolean;
   begin
      while Has_Element(I) loop
         declare
            Weight_Cur, Weight_Value : Kgs;
            Weight_Diff    : Float;
            Arm_Cur        : Arm;
            Arm_Diff       : Float;
            Arm_In_Range : Boolean := False;
            Limit : constant Cog_Limits_Entry := Element(I);
            Slope : Float;
         begin
            Weight_Cur := Limit.The_Weight;
            Arm_Cur := Limit.The_Arm;

            if The_Arm >= Arm_Prev and The_Arm <= Arm_Cur then
               Arm_In_Range := True;
            end if;
            if The_Arm >= Arm_Cur and The_Arm <= Arm_Prev then
               Arm_In_Range := True;
            end if;

            if Arm_In_Range then
               Weight_diff := Float(Weight_Cur - Weight_Prev);
               Arm_diff    := Float(Arm_Cur - Arm_Prev);
               Slope := Weight_Diff / Arm_Diff;
               Weight_Value := Kgs(Slope*Float(The_Arm - Arm_Prev))
                 + Weight_Prev;

               if Weight_Value > Weight_max then
                  Weight_Max := Weight_Value;
               end if;
               if Weight_Value < Weight_Min then
                  Weight_Min := Weight_Value;
               end if;
            end if;

            Weight_Prev := Weight_Cur;
            Arm_Prev := Arm_Cur;
         end;
         Next(I);
      end loop;
      Valid := The_Weight >= Weight_Min and The_Weight <= Weight_Max;
      return Valid;
   end Is_Cog_Within_Limits;

   procedure Check_Result(The_Wab : in Wab;
                          Max_Weight : in Kgs;
                          The_Result : in out Result) is
   begin
      if The_Result.The_Weight >= Max_Weight then
         Set_Cog_Entry_Status(The_Result.The_Status,Too_Heavy);
      end if;
      if The_Result.The_Type = Total then
         if not Is_Cog_Within_Limits(The_Wab,
                                     The_Result.The_Weight,
                                     The_Result.The_Arm) then
            Set_Cog_Entry_Status(The_Result.The_Status,Outside_Limits);
         end if;
      end if;
   end Check_Result;

   -- PUBLIC FUNCTIONS

   procedure Add_Cog_Entry(The_Wab : in out Wab;
                           The_Cog_Entry : in Cog_Entry) is
      Tmp : Cog_Entry := The_Cog_Entry;
   begin
--      Assert(Tmp.The_Type /= Total);
      Append(The_Wab.Cog_List,Tmp);
   end Add_Cog_Entry;

   procedure Set_Cog_Limits(The_Wab : in out Wab;
                            Cog_Limits : in Cog_Limits_Lists.Vector) is
   begin
      The_Wab.Cog_Limits := Cog_Limits;
   end Set_Cog_Limits;

   procedure Set_Weight_Limits(The_Wab : in out Wab;
                            The_Weight_Limits : in Weight_Limits) is
   begin
      The_Wab.The_Weight_Limits := The_Weight_Limits;
   end Set_Weight_limits;


   procedure Calculate(The_Wab : in out Wab) is
      I : Cog_Lists.Cursor := First(The_Wab.Cog_List);

      Subtotal_Weight : Kgs := 0;
      Subtotal_Moment : Moment := 0;
      SubTotal_Arm    : Arm := Arm'First;

      Fuel_Weight : Kgs := 0;
      Fuel_Moment : Moment := 0;
      Fuel_Arm    : Arm := Arm'First;

      Total_Weight : Kgs := 0;
      Total_Moment : Moment := 0;
      Total_Arm    : Arm := Arm'First;

   begin
      The_Wab.The_Status := Ok;

      ------------------------------
      -- FOR EVERY DEFINED WEIGHT --
      ------------------------------
      while Has_Element(I) loop
         declare
            V : constant Cog_Entry := Element(I);
            The_Moment : Moment := 0;
            The_Result : Result;
         begin
            -- calculate moment=weight*arm
            The_Moment := Moment(V.The_Weight) * Moment(V.The_Arm);

            -- calculate totals
            case V.The_Type is
               when Fuel =>
                 Fuel_Weight := Fuel_Weight + V.The_Weight;
                 Fuel_Moment := Fuel_Moment + The_Moment;
               when Total => null;
               when Luggage | Bew | Passenger =>
                  Subtotal_Weight := Subtotal_Weight + V.The_Weight;
                  Subtotal_Moment := Subtotal_Moment + The_Moment;
            end case;
            Total_Weight := Total_Weight + V.The_Weight;
            Total_Moment := Total_Moment + The_Moment;

            -- if this weight is not valid, then the result is not valid
            if V.The_Status /= Ok then
               Set_Cog_Entry_Status(The_Wab.The_Status,V.The_Status);
            end if;

            The_Result := (Description => V.Description,
                           The_Weight => V.The_Weight,
                           The_Arm => V.The_Arm,
                           The_Moment => The_Moment,
                           The_Type => V.The_Type,
                           The_Status => V.The_Status
                      );
            Check_Result(The_Wab,V.Max_Weight,The_Result);

            -- attach this item to list
	    if V.The_Type /= Total then
               Append(The_Wab.Results,The_Result);
            end if;
         end;

         -- go to next weight
         Next(I);
      end loop;

      --------------------------------------
      -- if subtotal valid add it to list --
      --------------------------------------
      if Subtotal_Weight /= 0 then
         Subtotal_Arm := Arm(Float(Subtotal_Moment)/Float(Subtotal_Weight));

         declare
            The_Result : Result :=
              (
               Description => To_Unbounded_String("SUBTOTAL"),
               The_Weight => Subtotal_Weight,
               The_Moment => Subtotal_Moment,
               The_Arm => Subtotal_Arm,
               The_Type => Total,
               The_Status => Ok
              );
         begin
            Check_Result(The_Wab,
                         The_Wab.The_Weight_Limits.Mzfw,
                         The_Result);
            Append(The_Wab.Results,The_Result);
            if The_Result.The_Status /= Ok then
               Set_Cog_Entry_Status(The_Wab.The_Status,The_Result.The_Status);
            end if;
         end;
      end if;

      ----------------------------------
      -- if fuel valid add it to list --
      ----------------------------------
      if Fuel_Weight /= 0 then
         Fuel_Arm := Arm(Float(Fuel_Moment)/Float(Fuel_Weight));

         declare
            The_Result : Result :=
              (
               Description => To_Unbounded_String("FUEL"),
               The_Weight => Fuel_Weight,
               The_Moment => Fuel_Moment,
               The_Arm => Fuel_Arm,
               The_Type => Total,
               The_Status => Ok
              );
         begin
            Append(The_Wab.Results,The_Result);
         end;
      end if;

      -----------------------------------
      -- if total valid add it to list --
      -----------------------------------
      if Total_Weight /= 0 then
         Total_Arm := Arm(Float(Total_Moment)/Float(Total_Weight));

         declare
            The_Result : Result :=
              (
               Description => To_Unbounded_String("TOTAL"),
               The_Weight => Total_Weight,
               The_Moment => Total_Moment,
               The_Arm => Total_Arm,
               The_Type => Total,
               The_Status => Ok
              );
         begin
            Check_Result(The_Wab,
                         The_Wab.The_Weight_Limits.Mtow,
                         The_Result);
            Append(The_Wab.Results,The_Result);
            if The_Result.The_Status /= Ok then
               Set_Cog_Entry_Status(The_Wab.The_Status,The_Result.The_Status);
            end if;
         end;
      end if;

   end Calculate;

   function Get_Status(The_Wab : in Wab) return Cog_Entry_Status is
   begin
      return The_Wab.The_Status;
   end Get_Status;

   function Get_Result_Iter(The_Wab : in Wab) return Result_Iter is
   begin
      return First(The_Wab.Results);
   end Get_Result_Iter;
end Wabs;
