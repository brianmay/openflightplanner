--                              -*- Mode: Ada -*-
-- Filename        : aircrafts.adb
-- Description     : Model an aircraft
-- Author          : Brian May
-- Created On      : Sat May  4 17:27:44 2002
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

package body Aircrafts is
   ----------------------------
   -- CONSTANT AIRCRAFT DATA --
   ----------------------------
   procedure Ensure_Data_Set(The_Aircraft : in Aircraft) is
   begin
      if not The_Aircraft.Data_Set then
         raise Not_Configured;
      end if;
   end Ensure_Data_Set;

   procedure Set_Aircraft_Data(The_Aircraft : in out Aircraft;
                               Data : in Aircraft_Data) is
   begin
      The_Aircraft.Data := Data;
      The_Aircraft.Data_Set := True;
   end Set_Aircraft_Data;

   function Get_Aircraft_Data(The_Aircraft : in Aircraft)
                             return Aircraft_Data is
   begin
      return The_Aircraft.Data;
   end Get_Aircraft_Data;


   function Convert_Fuel_To_Kgs(Qty : Litres)
                               return Kgs is
      The_Weight : Kgs;

   begin
      The_Weight := Kgs(Float(Qty)*0.72);
      return(The_Weight);
   end;

   function Get_Air_Speed(The_Aircraft : in Aircraft) return Air_Speed_Record is
   begin
      Ensure_Data_Set(The_Aircraft);
      return The_Aircraft.Data.Air_Speed;
   end Get_Air_Speed;

   function Get_Fuel_Consumption(The_Aircraft : in Aircraft)
                                return Fuel_Consumption_Record is
   begin
      return The_Aircraft.Data.Fuel_Consumption;
   end Get_Fuel_Consumption;

   function Get_Fuel_Used(The_Aircraft : in Aircraft;
                          Phase : in Flight_Phase;
                          Time : in Minutes) return Litres is
      subtype F is Float;
      Consumption : constant Litres :=
        The_Aircraft.Data.Fuel_Consumption(Phase);
   begin
      Ensure_Data_Set(The_Aircraft);
      return Litres(F(Time) * F(Consumption)/60.0);
   end Get_Fuel_Used;


   function Get_Time_Used(The_Aircraft : in Aircraft;
                          Phase : in Flight_Phase;
                          Fuel : in Litres) return Minutes is
      subtype F is Float;
      Consumption : constant Litres :=
        The_Aircraft.Data.Fuel_Consumption(Phase);
   begin
      Ensure_Data_Set(The_Aircraft);
      return Minutes(F(Fuel) * 60.0/F(Consumption));
   end Get_Time_Used;


   ----------------------------
   -- VARIABLE AIRCRAFT DATA --
   ----------------------------
   procedure Ensure_State_Set(The_Aircraft : in Aircraft) is
   begin
      if not The_Aircraft.State_Set then
         raise Not_Configured;
      end if;
   end Ensure_State_Set;

   procedure Assert_Aircraft_State(The_Aircraft : in Aircraft;
                                     State : in Aircraft_State) is
   begin
      if State.Fuel_Tank_List'Length
            /= The_Aircraft.Data.Fuel_Tank_List'Length then
         raise Wrong_Number_Of_Fuel_Tanks;
      end if;

      if State.Seat_List'Length
            /= The_Aircraft.Data.Seat_List'Length then
         raise Wrong_Number_Of_Seats;
      end if;
   end Assert_Aircraft_State;

   procedure Set_Aircraft_State(The_Aircraft : in out Aircraft;
                                State : in Aircraft_State) is
   begin
      Assert_Aircraft_State(The_Aircraft,State);
      for I in The_Aircraft.Data.Seat_List'Range loop
         declare
            S_Data : Seat renames The_Aircraft.Data.Seat_List(I);
            S_State : Seat_State renames State.Seat_List(I);
         begin
            if S_Data.The_Seat_Type = Not_A_Seat and
               S_State.Content /= Luggage then
                  raise Wrong_Loading;
            end if;
            if S_Data.The_Seat_Type = Always_A_Seat and
               S_State.Content /= Passenger then
                  raise Wrong_Loading;
            end if;
         end;
      end loop;
      The_Aircraft.State := State;
      The_Aircraft.State_Set := true;
   end Set_Aircraft_State;

   procedure Set_Aircraft_Empty_State(The_Aircraft : in out Aircraft) is
   begin
      for I in The_Aircraft.Data.Seat_List'Range loop
         declare
            S_Data : Seat renames The_Aircraft.Data.Seat_List(I);
            S_State : Seat_State renames The_Aircraft.State.Seat_List(I);
         begin
            if S_Data.The_Seat_Type = Not_A_Seat then
                  S_State := (
                     The_Weight => 0,
                     Content => Luggage
                  );
            else
                  S_State := (
                     The_Weight => 0,
                     Content => Passenger
                  );
            end if;
         end;
      end loop;
      for I in The_Aircraft.Data.Fuel_Tank_List'Range loop
         declare
            FT_State : Fuel_Tank_State
               renames The_Aircraft.State.Fuel_Tank_List(I);
         begin
            FT_State.Qty := 0;
        end;
      end loop;
      The_Aircraft.State_Set := true;
   end Set_Aircraft_Empty_State;

   procedure Assert_Seat_Change_Sane(S_Data : in Seat;
                                     S_Change : in Seat_Change) is
   begin
      if S_Data.The_Seat_Type = Not_A_Seat and
         S_Change.Content /= Luggage then
            raise Wrong_Loading;
      end if;
      if S_Data.The_Seat_Type = Always_A_Seat and
         S_Change.Content /= Passenger then
            raise Wrong_Loading;
      end if;
   end;

   procedure Update_Aircraft_State(The_Aircraft : in out Aircraft;
                                Change : in Aircraft_Change) is
   begin
      Ensure_State_Set(The_Aircraft);

      if Change.Fuel_Tank_List'Length
            > The_Aircraft.Data.Fuel_Tank_List'Length then
         raise Wrong_Number_Of_Fuel_Tanks;
      end if;

      if Change.Seat_List'Length
            > The_Aircraft.Data.Seat_List'Length then
         raise Wrong_Number_Of_Seats;
      end if;

      for I in Change.Seat_List'Range loop
         declare
            S_Change : Seat_Change renames Change.Seat_List(I);
            S_Data : Seat renames The_Aircraft.Data.Seat_List(I);
            S_State : Seat_State renames The_Aircraft.State.Seat_List(I);
         begin
            case S_Change.Change is
               when None =>
                  null;

               when Empty =>
                  Assert_Seat_Change_Sane(S_Data, S_Change);
                  S_State := (
                     The_Weight => 0,
                     Content => S_Change.Content
                  );

               when Full =>
                  Assert_Seat_Change_Sane(S_Data, S_Change);
                  if S_Change.The_Weight /= S_Data.Maximum then
                        raise Wrong_Loading;
                  end if;
                  S_State := (
                     The_Weight => S_Data.Maximum,
                     Content => S_Change.Content
                  );

               when Add =>
                  if S_State.Content /= S_Change.Content then
                        raise Wrong_Loading;
                  end if;
                  S_State.The_Weight := S_State.The_Weight
                     + S_Change.The_Weight;

               when Subtract =>
                  if S_State.Content /= S_Change.Content then
                        raise Wrong_Loading;
                  end if;
                  if S_State.The_Weight > S_Change.The_Weight then
                     S_State.The_Weight := S_State.The_Weight
                        - S_Change.The_Weight;
                  else
                     S_State.The_Weight := 0;
                  end if;

               when Replace =>
                  Assert_Seat_Change_Sane(S_Data, S_Change);
                  S_State := (
                     The_Weight => S_Change.The_Weight,
                     Content => S_Change.Content
                  );
            end case;
        end;
      end loop;

      for I in Change.Fuel_Tank_List'Range loop
         declare
            FT_Change : Fuel_Tank_Change
               renames Change.Fuel_Tank_List(I);
            FT_Data : Fuel_Tank
               renames The_Aircraft.Data.Fuel_Tank_List(I);
            FT_State : Fuel_Tank_State
               renames The_Aircraft.State.Fuel_Tank_List(I);
         begin
            case FT_Change.Change is
               when None =>
                  null;

               when Empty =>
                  FT_State.Qty := 0;

               when Full =>
                  if FT_Change.Qty /= FT_Data.Maximum then
                        raise Wrong_Loading;
                  end if;
                  FT_State.Qty := Ft_Data.Maximum;

               when Add =>
                  FT_State.Qty := FT_State.Qty + FT_Change.Qty;

               when Subtract =>
                  if FT_State.Qty > FT_Change.Qty then
                     FT_State.Qty := FT_State.Qty - FT_Change.Qty;
                  else
                     FT_State.Qty := 0;
                  end if;

               when Replace =>
                  FT_State := (
                     Qty => FT_Change.Qty
                  );
            end case;
        end;
      end loop;

   end Update_Aircraft_State;

   function Get_Aircraft_State(The_Aircraft : in Aircraft)
                              return Aircraft_State is
   begin
      Ensure_State_Set(The_Aircraft);
      return(The_Aircraft.State);
   end Get_Aircraft_State;

   procedure Consume_Fuel(The_Aircraft : in out Aircraft ;
                          Fuel : in Litres) is
      Tmp : Litres := Fuel;
   begin
      Ensure_State_Set(The_Aircraft);

      for I in The_Aircraft.Data.Fuel_Tank_List'Range loop
         declare
            Ft : Fuel_Tank renames
              The_Aircraft.Data.Fuel_Tank_List(I);
            Ftr : Fuel_Tank_State renames
              The_Aircraft.State.Fuel_Tank_List(I);
         begin
            if Tmp > Ftr.Qty then
               Tmp := Tmp - Ftr.Qty;
               Ftr.Qty := 0;
            else
               Ftr.Qty := Ftr.Qty - Tmp;
               Tmp := 0;
            end if;
         end;
      end loop;

      if Tmp > 0 then
         raise Not_Enough_Fuel;
      end if;
   end Consume_Fuel;

   function Get_Wab(The_Aircraft : in Aircraft) Return Wab is
      The_Wab : Wab;
   begin
      Ensure_Data_Set(The_Aircraft);
      Ensure_State_Set(The_Aircraft);

      Set_Cog_Limits(The_Wab,The_Aircraft.Data.Cog_Limits);
      Set_Weight_Limits(The_Wab,The_Aircraft.Data.The_Weight_Limits);

      Add_Cog_Entry(The_Wab,
                    ( Description => To_Unbounded_String("Basic Empty Weight"),
                      The_Weight => The_Aircraft.Data.Bew,
                      Max_Weight => Kgs'Last,
                      The_Arm => The_Aircraft.Data.Bew_Arm,
                      The_Type => bew,
                      The_Status => Ok
                    )
                   );

      for I in The_Aircraft.Data.Fuel_Tank_List'Range loop
         declare
            Ft : constant Fuel_Tank :=
              The_Aircraft.Data.Fuel_Tank_List(I);
            Ftr : constant Fuel_Tank_State :=
              The_Aircraft.State.Fuel_Tank_List(I);
            The_Weight : Kgs;
            The_Cog_Status : Cog_Entry_Status;
         begin
            if Ftr.Qty > Ft.Maximum then
               The_Cog_Status := Too_Much;
            else
               The_Cog_Status := Ok;
            end if;

            The_Weight := Convert_Fuel_To_Kgs(Ftr.Qty);
            Add_Cog_Entry(The_Wab,
                          ( Description => Ft.Description,
                            The_Weight => The_Weight,
                            Max_Weight => Kgs'Last,
                            The_Arm => Ft.The_Arm,
                            The_Type => Fuel,
                            The_Status => The_Cog_Status
                          )
                         );
         end;
      end loop;

      for I in The_Aircraft.Data.Seat_List'Range loop
         declare
            S : constant Seat :=
              The_Aircraft.Data.Seat_List(I);
            Sr : constant Seat_State :=
              The_Aircraft.State.Seat_List(I);
            The_Cog_Type : Cog_Entry_Type;
         begin
            case Sr.Content is
               when Passenger =>
                  The_Cog_Type := Passenger;
               when Luggage =>
                  The_Cog_Type := Luggage;
            end case;

            Add_Cog_Entry(The_Wab,
                          ( Description => S.Description,
                            The_Weight => Sr.The_Weight,
                            Max_Weight => S.Maximum,
                            The_Arm => S.The_Arm,
                            The_Type => The_Cog_Type,
                            The_Status => Ok
                          )
                         );
         end;
      end loop;

      Calculate(The_Wab);
      return(The_Wab);
   end Get_Wab;

   function Get_Total_Fuel(The_Aircraft : in Aircraft) return Litres is
     Fuel : Litres := 0;
   begin
      Ensure_Data_Set(The_Aircraft);
      Ensure_State_Set(The_Aircraft);

      -- note: it is an error to have fuel in fuel tanks which don't exist!
      for I in The_Aircraft.Data.Fuel_Tank_List'Range loop
         declare
            Ft : constant Fuel_Tank :=
              The_Aircraft.Data.Fuel_Tank_List(I);
            Ftr : constant Fuel_Tank_State :=
              The_Aircraft.State.Fuel_Tank_List(I);
         begin
            Fuel := Fuel + Ftr.Qty;
         end;
      end loop;
      return(Fuel);
   end Get_Total_Fuel;

end Aircrafts;
