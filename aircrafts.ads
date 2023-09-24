--                              -*- Mode: Ada -*-
-- Filename        : aircrafts.ads
-- Description     : Model an aircraft.
-- Author          : Brian May
-- Created On      : Sat May  4 17:27:29 2002
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

with Wabs;
use Wabs;

with Units;
use Units;

package Aircrafts is

   -- misc information
   type Wake_Type is (Heavy, Medium, Light);
   type SSR_Type is (None,Mode_A,Mode_C);

   type Equipment is
      record
         DME, GNSS, INS, ILS : Boolean := False;
         RNP, UHF, ADF, VOR : Boolean := False;
         HF, DLink, TAC, VHF : Boolean := False;
         GPS, ADS : Boolean := False;
         SSR : SSR_Type := None;
      end record;

   -- aircraft performance
   type Air_Speed_Record is
      record
         Cruise, Climb, Descend : Knots := 0;
      end record;

   -- different phases of flight
   type Litres is range 0..500;
   -- do not change the order of cruise..descend
   type Flight_Phase is (Climb,Cruise,Descend,Holding,Taxi,Awk);
   type Time_Record is array (Flight_Phase) of Minutes;
   type Fuel_Record is array (Flight_Phase) of Litres;

   -- fuel consumption
   type Fuel_Consumption_Record is array (Flight_Phase) of Litres;

   -- loading
   type Change_Type is (None, Empty, Full, Replace, Add, Subtract);

   -- fuel tanks
   type Fuel_Tank is
      record
         Description  : Unbounded_String;
         The_Arm      : Arm;
         Maximum      : Litres;
      end record;

   type Fuel_Tank_State is
      record
         Qty      : Litres;
      end record;

   type Fuel_Tank_Change is
      record
         Change   : Change_Type;
         Qty      : Litres;
      end record;

   -- laggage/passenger seats
   type Seat_Type is (Not_A_Seat,Always_A_Seat,Removable_Seat);
   type Seat is
      record
         Description   : Unbounded_String;
         The_Seat_Type : Seat_Type;
         The_Arm       : Arm;
         Maximum       : Kgs;
      end record;

   type Seat_Content is (Passenger,Luggage);
   type Seat_State is
      record
         The_Weight : Kgs;
         Content    : Seat_Content;
      end record;

   type Seat_Change is
      record
         Change     : Change_Type;
         The_Weight : Kgs;
         Content    : Seat_Content;
      end record;

   -- arrays
   type Fuel_Tank_Array is array (Positive range <>) of Fuel_Tank;
   type Fuel_Tank_State_Array is array (Positive range <>) of Fuel_Tank_State;
   type Fuel_Tank_Change_Array is array (Positive range <>) of Fuel_Tank_Change;
   type Seat_Array is array (Positive range <>) of Seat;
   type Seat_State_Array is array (Positive range <>) of Seat_State;
   type Seat_Change_Array is array (Positive range <>) of Seat_Change;

   -- all aircraft data
   type Aircraft_Data(Fuel_Tanks : Positive; Seats : Positive) is
      record
         Callsign,
         Full_Callsign        : Unbounded_String;
         The_Type             : Unbounded_String;
         Approved_Type        : Boolean;
         Colour               : Unbounded_String;
         Description          : Unbounded_String;
         The_Equipment        : Equipment;
         The_Wake_Type        : Wake_Type;
         Fuel_Consumption     : Fuel_Consumption_Record;
         Air_Speed            : Air_Speed_Record;
         The_Weight_Limits    : Weight_Limits;
         Fuel_Tank_List       : Fuel_Tank_Array(1..Fuel_Tanks);
         Seat_List            : Seat_Array(1..Seats);
         Cog_Limits           : Cog_Limits_Lists.Vector;
         Bew                  : Kgs;
         Bew_Arm              : Arm;
      end record;

   type Aircraft_State(Fuel_Tanks : Positive; Seats : Positive) is
      record
         Fuel_Tank_List : Fuel_Tank_State_Array(1..Fuel_Tanks);
         Seat_List      : Seat_State_Array(1..Seats);
      end record;

   type Aircraft_Change(Fuel_Tanks : Natural; Seats : Natural) is
      record
         Fuel_Tank_List : Fuel_Tank_Change_Array(1..Fuel_Tanks);
         Seat_List      : Seat_Change_Array(1..Seats);
      end record;

   -- private object
   type Aircraft(Fuel_Tanks : Positive; Seats : Positive) is tagged private;
   type Aircraft_Access is access Aircraft;

   -- exceptions
   Not_Enough_Fuel : exception;
   Not_Configured : exception;
   Wrong_Number_Of_Fuel_Tanks : exception;
   Wrong_Number_Of_Seats : exception;
   Wrong_Loading : exception;

   ----------------------------
   -- CONSTANT AIRCRAFT DATA --
   ----------------------------
   procedure Set_Aircraft_Data(The_Aircraft : in out Aircraft;
                               Data : in Aircraft_Data);
   function Get_Aircraft_Data(The_Aircraft : in Aircraft)
                             return Aircraft_Data;

   function Get_Air_Speed(The_Aircraft : in Aircraft) return Air_Speed_Record;
   function Get_Fuel_Consumption(The_Aircraft : in Aircraft)
                                return Fuel_Consumption_Record;

   function Get_Fuel_Used(The_Aircraft : in Aircraft;
                          Phase : in Flight_Phase;
                          Time : in Minutes) return Litres;

   function Get_Time_Used(The_Aircraft : in Aircraft;
                          Phase : in Flight_Phase;
                          Fuel : in Litres) return Minutes;

   ----------------------------
   -- VARIABLE AIRCRAFT DATA --
   ----------------------------
   procedure Set_Aircraft_Empty_State(The_Aircraft : in out Aircraft);
   procedure Set_Aircraft_State(The_Aircraft : in out Aircraft;
                                State : in Aircraft_State);
   procedure Update_Aircraft_State(The_Aircraft : in out Aircraft;
                                Change : in Aircraft_Change);

   procedure Assert_Aircraft_State(The_Aircraft : in Aircraft;
                                   State : in Aircraft_State);

   function Get_Aircraft_State(The_Aircraft : in Aircraft)
                                return Aircraft_State;

   function Get_Wab(The_Aircraft : in Aircraft) Return Wab;

   function Get_Total_Fuel(The_Aircraft : in Aircraft) return Litres;

   procedure Consume_Fuel(The_Aircraft : in out Aircraft ;
                          Fuel : in Litres);

private
   type Aircraft(Fuel_Tanks : Positive; Seats : Positive)
                is tagged
      record
         Data         : Aircraft_Data(Fuel_Tanks, Seats);
         Data_Set     : Boolean        := False;
         State        : Aircraft_State(Fuel_Tanks, Seats);
         State_Set    : Boolean        := False;
      end record;

end Aircrafts;
