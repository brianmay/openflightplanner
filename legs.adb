--                              -*- Mode: Ada -*-
-- Filename        : legs.adb
-- Description     : One leg in flight in flightplan
-- Author          : Brian May
-- Created On      : Sat May  4 17:19:57 2002
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
with Ada.Text_Io;
use Ada.Text_Io;

package body Legs is
   subtype F is Float;

   function Get_Fuel_Used(The_Leg : in Leg;
                          Phase : in Flight_Phase;
                          Time : in F) return F is
      Fuel_Consumption : constant Fuel_Consumption_Record
        := The_Leg.Fuel_Consumption;
   begin
      return Time * F(Fuel_Consumption(Phase))/60.0;
   end Get_Fuel_Used;


   -- Note:
   --
   -- Each leg split up into three parts (as well as holding,awk,taxi):
   -- 1. start: initial climb/descend.
   -- 2. rest: cruise.
   -- 3. end: descend/descend.
   --
   -- This allows precise control if descent/climb should start before
   -- or after going to new leg.
   --
   -- Floats are used to be more accurate then integers.

   procedure Update(The_Leg : in out Leg) is

      Air_Speed : Air_Speed_Record renames The_Leg.Air_Speed;
      A : constant Aircraft_Access := The_Leg.The_Aircraft;

      Start_Phase : Flight_Phase range Climb..Descend;
      Start_Diff : F;
      Start_Time : F;
      Start_Distance : F;
      Start_Direction : Degrees;
      Start_Speed : Knots;
      Start_Fuel : F;

      End_Phase : Flight_Phase range Climb..Descend;
      End_diff : F;
      End_Time : F;
      End_Distance : F;
      End_Direction : Degrees;
      End_Speed : Knots;
      End_Fuel : F;

      Rest_Distance : F;
      Rest_Direction : Degrees;
      Rest_Speed : Knots;

      Time_Cruise, Time_Descend, Time_Climb : F := 0.0;
      Fuel_Cruise, Fuel_Descend, Fuel_Climb : F := 0.0;

      Time : Time_Used_Record renames The_Leg.Time_Used;
      Fuel : Fuel_Used_Record renames The_Leg.Fuel_Used;

   begin
      Put_Line("============= "&To_String(The_Leg.Destination.Name)&
               " =============");
      if not The_Leg.Configured_Aircraft then
         raise Not_Configured;
      end if;

      if not The_Leg.Uptodate then
         Time := (others => 0);
         Fuel := (others => 0);

         Air_Speed := The_Leg.Air_Speed;

         -- calculate difference in hieghts
         Start_Diff := F(The_Leg.Alt - The_Leg.Alt_Start);
         End_Diff   := F(The_Leg.Alt_End - The_Leg.Alt);

         -- calculate distance and time when climbing or descending at start
         if Start_Diff < 0.0 then
            Start_Phase := Descend;
            Start_Diff := -Start_Diff;
            Start_Speed := Air_Speed.Descend;
            Start_Time := Start_Diff / Descend_Rate;
         else
            Start_Phase := Climb;
            Start_Speed := Air_Speed.Climb;
            Start_Time := Start_Diff / Climb_Rate;
         end if;
         Start_Direction := The_Leg.Track;
         Compensate_For_Wind(The_Leg.The_Wind,Start_Direction,Start_Speed);
         Start_Distance := F(Start_Speed) * Start_Time / 60.0;
         Start_Fuel := Get_Fuel_Used(The_Leg,Start_Phase,Start_Time);
         case Start_Phase is
            when Descend =>
               Time_Descend := Time_Descend + Start_Time;
               Fuel_Descend := Fuel_Descend + Start_Fuel;
            when Climb =>
               Time_Climb := Time_Climb + Start_Time;
               Fuel_Climb := Fuel_Climb + Start_Fuel;
            when Cruise =>
               -- should never happen
               null;
         end case;

         Put_Line("START(alt+"&F'Image(Start_Diff)&", "
                  &"*distance="&F'Image(Start_Distance)&","
                  &"*fuel="&F'Image(Start_Fuel)&", "
                  &"speed="&Knots'Image(Start_Speed)&", "
                  &"*direction="&Degrees'Image(Start_Direction)&", "
                  &"*eti="&F'Image(Start_Time)&", "
                  &"phase="&Flight_Phase'Image(Start_Phase)
                  &")");

         -- calculate distance and time when climbing or descending at end
         if End_Diff < 0.0 then
            End_Phase := Descend;
            End_Diff := -End_Diff;
            End_Speed := Air_Speed.Descend;
            End_Time := End_Diff / Descend_Rate;
         else
            End_Phase := Climb;
            End_Speed := Air_Speed.Climb;
            End_Time := End_Diff / Climb_Rate;
         end if;
         End_Direction := The_Leg.Track;
         Compensate_For_Wind(The_Leg.The_Wind,End_Direction,End_Speed);
         End_Distance := F(End_Speed) * End_Time / 60.0;
         End_Fuel := Get_Fuel_Used(The_Leg,End_Phase,End_Time);
         case End_Phase is
            when Descend =>
               Time_Descend := Time_Descend + End_Time;
               Fuel_Descend := Fuel_Descend + End_Fuel;
            when Climb =>
               Time_Climb := Time_Climb + End_Time;
               Fuel_Climb := Fuel_Climb + End_Fuel;
            when Cruise =>
               -- should never happen
               null;
         end case;

         -- debug

         Put_Line("END(alt+"&F'Image(End_Diff)&", "
                  &"*distance="&F'Image(End_Distance)&", "
                  &"*fuel="&F'Image(End_Fuel)&", "
                  &"speed="&Knots'Image(End_Speed)&", "
                  &"*direction="&Degrees'Image(End_Direction)&", "
                  &"*eti="&F'Image(End_Time)&", "
                  &"phase="&Flight_Phase'Image(End_Phase)
                  &")");

         -- calculate distance and time for remander of leg
         Rest_Speed := Air_Speed.Cruise;
         Rest_Direction := The_Leg.Track;
         Compensate_For_Wind(The_Leg.The_Wind, Rest_direction, Rest_Speed);
         Rest_Distance  := F(The_Leg.Distance) - Start_Distance - End_Distance;
         Time_Cruise := Rest_Distance/F(Rest_Speed)*60.0;
         Fuel_Cruise := Get_Fuel_Used(The_Leg,Cruise,Time_Cruise);

         Put_Line("REST(alt="&Alt_Feet'Image(The_Leg.Alt)&", "
                  &"*distance="&F'Image(Rest_Distance)&", "
                  &"*fuel="&F'Image(Fuel_Cruise)&","
                  &"*speed="&Knots'Image(Rest_Speed)&", "
                  &"*direction="&Degrees'Image(Rest_Direction)&", "
                  &"*eti="&F'Image(Time_Cruise)
                  &")");

         -- translate to correct types;

         Time(Descend) := Minutes(Time_Climb);
         Fuel(Descend) := Litres(Fuel_Climb);

         Time(Cruise) := Minutes(Time_Cruise);
         Fuel(Cruise) := Litres(Fuel_Cruise);

         Time(Climb) := Minutes(Time_Climb);
         Fuel(Climb) := Litres(Fuel_Climb);

         -- awk
         Time(Awk) := The_Leg.Awk_Time;
         Fuel(Awk) := Get_Fuel_Used(A.all,Awk,Time(Awk));

         Put_Line("AWK("
                  &"*fuel="&Litres'Image(Fuel(Awk))&","
                  &"*eti="&Minutes'Image(Time(Awk))
                  &")");

         -- taxi
         Time(Taxi) := The_Leg.Taxi_Time;
         Fuel(Taxi) := Get_Fuel_Used(A.all,Taxi,Time(Taxi));

         Put_Line("TAXI("
                  &"*fuel="&Litres'Image(Fuel(Taxi))&","
                  &"*eti="&Minutes'Image(Time(Taxi))
                  &")");

         -- holding
         Time(Holding) := The_Leg.Holding_Time;
         Fuel(Holding) := Get_Fuel_Used(A.all,Holding,Time(Holding));

         Put_Line("HOLDING("
                  &"*fuel="&Litres'Image(Fuel(Holding))&","
                  &"*eti="&Minutes'Image(Time(Holding))
                  &")");

         The_Leg.True_Air_Speed := Air_Speed.Cruise;
         The_Leg.Ground_Speed := Rest_Speed;
         The_Leg.Heading := Rest_Direction;
         The_Leg.Eti := Minutes(Start_Time + F(Time(Cruise)) + End_Time
                                + F(Time(Awk)) + F(Time(Taxi))
                                + F(Time(Holding)));

         Put_Line("TOTAL("
                  &"*eti="&Minutes'Image(The_Leg.Eti)
                  &")");

         The_Leg.Uptodate := True;
      end if;
   end;

   -- sub programs
   procedure Initialize(The_Leg : out Leg;
                        Lowest_Safe_Alt, Alt : in Alt_Feet;
                        Track : in Degrees;
                        Distance : in N_Miles;
                        Awk_Time : in Minutes;
                        Awk_Rmk : in Unbounded_String;
                        Holding_Time : in Minutes;
                        Taxi_Time : in Minutes) is
      Dummy : Waypoint;
   begin
      The_Leg := (Lowest_Safe_Alt => Lowest_Safe_Alt,
                  Alt => Alt,
                  Track => Track,
                  Distance => Distance,
                  Awk_Time => Awk_Time,
                  Awk_Rmk => Awk_Rmk,
                  Holding_Time => Holding_Time,
                  Taxi_Time => Taxi_Time,
                  Uptodate => False,
                  Configured_Aircraft => False,

		  Destination => Dummy,
                  Alt_Start => Alt,
                  Alt_End => Alt,
                  Air_Speed => ( others => 0 ),
                  Fuel_Consumption => ( others => 0),
                  True_Air_Speed => 0,
                  The_Wind => ( No_Wind ),
                  Heading => 0,
                  Ground_Speed => 0,
                  Eti => 0,
                  Time_Used => ( others => 0),
                  Fuel_Used => ( others => 0 ),
                  The_Aircraft => null
                 );
   end Initialize;

   procedure Set_Destination(The_Leg : in out Leg;
                             Destination : in Waypoint) is
   begin
      The_Leg.Destination := Destination;
   end Set_Destination;

   procedure Set_Aircraft(The_Leg : in out Leg;
                          The_Aircraft : in Aircraft_Access) is
   begin
      The_Leg.The_Aircraft := The_Aircraft;
      The_Leg.Air_Speed := Get_Air_Speed(The_Aircraft.all);
      The_Leg.Fuel_Consumption := Get_Fuel_Consumption(The_Aircraft.all);
      The_Leg.Uptodate := False;
      The_Leg.Configured_Aircraft := True;
   end;

   procedure Set_Alt_Start(The_Leg : in out Leg;
                           Alt : in Alt_Feet) is
   begin
      The_Leg.Alt_Start := Alt;
      The_Leg.Uptodate := False;
   end Set_Alt_Start;


   procedure Set_Alt_End(The_Leg : in out Leg;
                         Alt : in Alt_Feet) is
   begin
      The_Leg.Alt_End := Alt;
      The_Leg.Uptodate := False;
   end Set_Alt_End;

   procedure Set_Wind(The_Leg : in out Leg;
                      The_Wind : in Wind) is
   begin
      The_Leg.The_Wind := The_Wind;
      The_Leg.Uptodate := False;
   end;

   -- fixed data
   function Get_Destination(The_Leg : in Leg) Return Waypoint is
   begin
      return The_Leg.Destination;
   end Get_Destination;

   function Get_Lowest_Safe_alt(The_Leg : in Leg) return Alt_Feet is
   begin
      return The_Leg.Lowest_Safe_Alt;
   end Get_Lowest_Safe_Alt;

   function Get_Alt(The_Leg : in Leg) return Alt_Feet is
   begin
      return The_Leg.Alt;
   end Get_Alt;

   function Get_Track(The_Leg : in Leg) return Degrees is
   begin
      return The_Leg.Track;
   end Get_Track;

   function Get_Distance(The_Leg : in Leg) return N_Miles is
   begin
      return The_Leg.Distance;
   end Get_Distance;

   function Get_Awk_Time(The_Leg : in Leg) return Minutes is
   begin
      return The_Leg.Awk_Time;
   end Get_Awk_Time;

   function Get_Awk_Rmk(The_Leg : in Leg) return Unbounded_String is
   begin
      return The_Leg.Awk_Rmk;
   end Get_Awk_Rmk;

   function Get_Holding_Time(The_Leg : in Leg) return Minutes is
   begin
      return The_Leg.Holding_Time;
   end Get_Holding_Time;

   function Get_Alt_Start(The_Leg : in Leg) return Alt_Feet is
   begin
      return The_Leg.Alt_Start;
   end Get_Alt_Start;

   function Get_Alt_End(The_Leg : in Leg) return Alt_Feet is
   begin
      return The_Leg.Alt_End;
   end Get_Alt_End;

   -- determined from aircraft
   procedure Ensure_Uptodate(The_Leg : in Leg) is
   begin
      if not The_Leg.Uptodate then
         raise Not_Uptodate;
      end if;
   end Ensure_Uptodate;

   function Get_True_Air_Speed(The_Leg : in Leg) return Knots is
   begin
      Ensure_Uptodate(The_Leg);
      return The_Leg.True_Air_Speed;
   end Get_True_Air_Speed;

   -- determined from wind
   function Get_Wind_Speed(The_Leg : in Leg) return Knots is
   begin
      return Get_Speed(The_Leg.The_Wind);
   end Get_Wind_speed;

   function Get_Wind_Dir(The_Leg : in Leg) return Degrees is
   begin
      return Get_Direction(The_Leg.The_Wind);
   end Get_Wind_Dir;

   function Get_Heading(The_Leg : in Leg) return Degrees is
   begin
      Ensure_Uptodate(The_Leg);
      return The_Leg.Heading;
   end Get_Heading;

   function Get_Ground_Speed(The_Leg : in Leg) return Knots is
   begin
      Ensure_Uptodate(The_Leg);
      return The_Leg.Ground_Speed;
   end Get_Ground_Speed;

   function Get_Eti(The_Leg : in Leg) return Minutes is
   begin
      Ensure_Uptodate(The_Leg);
      return The_Leg.Eti;
   end Get_Eti;

   function Get_Time_Used(The_Leg : in Leg) return Time_Used_Record is
   begin
      Ensure_Uptodate(The_Leg);
      return The_Leg.Time_Used;
   end Get_Time_Used;

   function Get_Fuel_Used(The_Leg : in Leg) return Fuel_Used_Record is
   begin
      Ensure_Uptodate(The_Leg);
      return The_Leg.Fuel_Used;
   end Get_Fuel_Used;

end Legs;
