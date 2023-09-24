--                              -*- Mode: Ada -*-
-- Filename        : xml.adb
-- Description     : XML File Reading/Writing
-- Author          : Brian May
-- Created On      : Wed May 15 16:27:00 2002
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
with Text_IO; use Text_IO;

with Input_Sources.File;
use Input_Sources.File;

with DOM.Readers;
use DOM.Readers;

with Sax.Readers;
use Sax.Readers;

with DOM.Core;
use DOM.Core;

with DOM.Core.Nodes;
use DOM.Core.Nodes;

with DOM.Core.Documents;
use DOM.Core.Documents;

with DOM.Core.Elements;
use DOM.Core.Elements;

with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Exceptions;
use Ada.Exceptions;

with Ada.Calendar;
use Ada.Calendar;

with Wabs;
use Wabs;
use Wabs.Cog_Limits_Lists;

with Units;
use Units;

with Waypoints;
use Waypoints;

with Winds;
use Winds;

with Maps;

package body Xml is
   use Aircrafts;
   use Legs;
   use Flights;
   use Flightplans;

   subtype DOM_Element is DOM.Core.Element;

   -----------------------------------------------------------------
   -- GIVEN A PARENT ELEMENT FIND INFORMATION ABOUT LEAF ELEMENTS --
   -----------------------------------------------------------------

   -- GET COUNT OF ELEMENTS WITH GIVEN NAME
   function Get_Count_Of_Elements(Root : in DOM_Element;
                                  Name : in DOM_String
                                 ) return Natural;

   -- GET THE VALUE FOR ELEMENT NAME Name
   type Value_Option is (optional);
   generic
      type Data_Type is private;
      with function Get_Value(Root : in DOM_Element) return Data_Type;
   function Get_Single_Value(Root : in DOM_Element;
                             Name : in DOM_String;
                             Option : in Value_Option;
                             Default : in Data_Type
                             ) return Data_Type;
   generic
      type Data_Type is private;
      with function Get_Value(Root : in DOM_Element) return Data_Type;
   function Get_Required_Single_Value(Root : in DOM_Element;
                                      Name : in DOM_String
                                     ) return Data_Type;

   -- GET ARRAY CONTAINING VALUES FROM EACH ELEMENT NAME Name
   generic
      type Data_Type is private;
      type Data_Array is array (Positive range <>) of Data_Type;
      with function Get_Value(Root : in DOM_Element) return Data_Type;
   function Get_Array(Root : in DOM_Element;
                             Name : in DOM_String;
                             Min : in Natural
                             ) return Data_Array;

   -- GET COUNT OF ELEMENTS WITH GIVEN NAME
   function Get_Count_Of_Elements(Root : in DOM_Element;
                                  Name : in DOM_String
                                 ) return Natural is
      My_Nodes : Node_List := Child_Nodes(Root);
      Given : Natural := 0;
   begin
      My_Nodes := Child_Nodes(Root);
      for i in 0 .. Length(My_Nodes)-1  loop
         declare
            My_Node : Node renames Item(My_Nodes,i);
         begin
            if My_Node.Node_Type = Element_Node then
               if Node_Name(My_Node) = Name then
                  Given := Given + 1;
               end if;
            end if;
         end;
      end loop;
      return Given;
   end Get_Count_Of_Elements;

   -- GET THE VALUE FOR ELEMENT NAME Name
   function Get_Single_Value(Root : in DOM_Element;
                             Name : in DOM_String;
                             Option : in Value_Option;
                             Default : in Data_Type
                             ) return Data_Type is
      Value : Data_Type := Default;
      My_Nodes : Node_List := Child_Nodes(Root);
      Given : Natural := 0;
   begin
      My_Nodes := Child_Nodes(Root);
      for i in 0 .. Length(My_Nodes)-1  loop
         declare
            My_Node : Node renames Item(My_Nodes,i);
         begin
            if My_Node.Node_Type = Element_Node then
               if Node_Name(My_Node) = Name then
                  Value := Get_Value(My_Node);
                  Given := Given + 1;
               end if;
            end if;
         end;
      end loop;

      if Given>1 then
         Raise_Exception(Invalid_XML'Identity,"The value "&Name&" was "&
                         "given multiple times");
      end if;

      return Value;
   end Get_Single_Value;

   function Get_Required_Single_Value(Root : in DOM_Element;
                                      Name : in DOM_String
                                     ) return Data_Type is
      Value : Data_Type;
      My_Nodes : Node_List := Child_Nodes(Root);
      Given : Natural := 0;
   begin
      My_Nodes := Child_Nodes(Root);
      for i in 0 .. Length(My_Nodes)-1  loop
         declare
            My_Node : Node renames Item(My_Nodes,i);
         begin
            if My_Node.Node_Type = Element_Node then
               if Node_Name(My_Node) = Name then
                  Value := Get_Value(My_Node);
                  Given := Given + 1;
               end if;
            end if;
         end;
      end loop;

      if Given=0 then
         Raise_Exception(Invalid_XML'Identity,"The value "&Name&" was "&
                            "required but not given");
      end if;

      if Given>1 then
         Raise_Exception(Invalid_XML'Identity,"The value "&Name&" was "&
                         "given multiple times");
      end if;

      return Value;
   end Get_Required_Single_Value;

   -- GET ARRAY CONTAINING VALUES FROM EACH ELEMENT NAME Name
   function Get_Array(Root : in DOM_Element;
                             Name : in DOM_String;
                             Min : in Natural
                             ) return Data_Array is
      My_Nodes : Node_List := Child_Nodes(Root);
      Given : Natural := 0;

      Count : Natural := Get_Count_Of_Elements(Root,Name);
      Data : Data_Array(1..Count);
      j : Positive := Data'First;
   begin
      My_Nodes := Child_Nodes(Root);
      for i in 0 .. Length(My_Nodes)-1  loop
         declare
            My_Node : Node renames Item(My_Nodes,i);
         begin
            if My_Node.Node_Type = Element_Node then
               if Node_Name(My_Node) = Name then
                  Data(j) := Get_Value(My_Node);
                  j := j + 1;
                  Given := Given + 1;
               end if;
            end if;
         end;
      end loop;

      if Count < Min then
         Raise_Exception(Invalid_XML'Identity,"The value "&Name&" was "&
                            "required "&Natural'Image(Min)&
                            " times but only given "&
                            Natural'Image(Given)&" times.");
      end if;

      pragma assert(Given = Data'Length,"Wrong number of items");
      return Data;
   end Get_Array;

   ---------------------------------------------------------------------
   -- GIVEN A PARENT ELEMENT FIND INFORMATION ABOUT NON-LEAF ELEMENTS --
   ---------------------------------------------------------------------
   generic
      type Data_Type is private;
      type Data_Array is array (Positive range <>) of Data_Type;
      with function Get_Array(Root : in DOM_Element;
                              Name : in DOM_String;
                              Min  : in Natural) return Data_Array;
   function Get_Single_Array(Root : in DOM_Element;
                             Name : in DOM_String;
                             Child_Name : in DOM_String;
                             Min : in Natural
                             ) return Data_Array;

   function Get_Single_Array(Root : in DOM_Element;
                             Name : in DOM_String;
                             Child_Name : in DOM_String;
                             Min : in Natural
                             ) return Data_Array is
      My_Nodes : Node_List := Child_Nodes(Root);
      The_Node : Node;
      Given : Natural := 0;
   begin
      My_Nodes := Child_Nodes(Root);
      for i in 0 .. Length(My_Nodes)-1  loop
         declare
            My_Node : Node renames Item(My_Nodes,i);
         begin
            if My_Node.Node_Type = Element_Node then
               if Node_Name(My_Node) = Name then
                  The_Node := My_Node;
                  Given := Given + 1;
               end if;
            end if;
         end;
      end loop;

      if Given=0 and Min > 0 then
         Raise_Exception(Invalid_XML'Identity,"The value "&Name&" was "&
                            "required but not given");
      end if;

      if Given>1 then
         Raise_Exception(Invalid_XML'Identity,"The value "&Name&" was "&
                         "given multiple times");
      end if;

      if Given = 0 then
         declare
            Empty : Data_Array(1..0);
         begin
            return Empty;
         end;
      else
         return Get_Array(The_Node,Child_Name,Min);
      end if;
   end Get_Single_Array;

   -- GET A STRING FROM THIS ELEMENT
   function Get_String(Root : in DOM_Element) return DOM_String is
      My_Nodes : Node_List;
   begin
      My_Nodes := Child_Nodes(Root);

      for i in 0 .. Length(My_Nodes)-1  loop
         declare
            My_Node : Node renames Item(My_Nodes,i);
         begin
            case My_Node.Node_Type is
               when Text_Node =>
                  return Node_Value(My_Node);
               when others =>
                  null;
            end case;
         end;
      end loop;
      return "";
   end Get_String;

   ----------------------------------------------------------
   -- GIVEN A LEAF ELEMENT FIND INFORMATION ABOUT CONTENTS --
   ----------------------------------------------------------

   -- GET A DISCRETE VALUE FROM THIS ELEMENT
   generic
      type Data_Type is (<>);
   function Get_Discrete(Root : in DOM_Element) return Data_Type;

   -- GET A FIXED POINT FROM THIS ELEMENT
   generic
      type Data_Type is delta <>;
   function Get_Fixed_Point(Root : in DOM_Element) return Data_Type;

   -- GET A STRING FROM THIS ELEMENT
   function Get_String(Root : in DOM_Element) return Unbounded_String;

   -- GET A BOOLEAN FROM THIS ELEMENT
   function Get_Boolean(Root : in DOM_Element) return Boolean;

   -- GET A DISCRETE VALUE FROM THIS ELEMENT
   function Get_Discrete(Root : in DOM_Element) return Data_Type is
      S : DOM_String := Get_String(Root);
   begin
      return Data_Type'Value(S);
   exception
      when Constraint_Error =>
         Raise_Exception(Invalid_XML'Identity,"The element "&Node_Name(Root)&
                         " with the value of "&S&
                         " could not be interpreted");
   end Get_Discrete;

   -- GET A FIXED POINT FROM THIS ELEMENT
   function Get_Fixed_Point(Root : in DOM_Element) return Data_Type is
      S : DOM_String := Get_String(Root);
   begin
      return Data_Type'Value(S);
   exception
      when Constraint_Error =>
         Raise_Exception(Invalid_XML'Identity,"The element "&Node_Name(Root)&
                         " with the value of "&S&
                         " could not be interpreted");
   end Get_Fixed_Point;

   -- GET A UNBOUNDED STRING FROM THIS ELEMENT
   function Get_String(Root : in DOM_Element) return Unbounded_String is
   begin
      return To_Unbounded_String(Get_String(Root));
   end Get_String;

   -- GET A BOOLEAN VALUE FROM THIS ELEMENT
   function Get_Boolean(Root : in DOM_Element) return Boolean is
   begin
      return true;
   end Get_Boolean;

   --------------------------------------------
   -- CREATE INSTANCES OF GENERIC PROCEDURES --
   --------------------------------------------

   -- REQUIRED INSTANCES OF ABOVE GENERIC PROCEDURES
   function Get_Alt     is new Get_Discrete(Alt_Feet);
   function Get_Arm     is new Get_Fixed_Point(Arm);
   function Get_Change_Type  is new Get_Discrete(Change_Type);
   function Get_Degrees is new Get_Discrete(Degrees);
   function Get_Kgs     is new Get_Discrete(Kgs);
   function Get_Knots   is new Get_Discrete(Knots);
   function Get_Litres  is new Get_Discrete(Litres);
   function Get_Minutes is new Get_Discrete(Minutes);
   function Get_N_Miles is new Get_Discrete(N_Miles);
   function Get_Seat    is new Get_Discrete(Seat_Type);
   function Get_Seat_Content is new Get_Discrete(Seat_Content);
   function Get_SSR     is new Get_Discrete(SSR_Type);
   function Get_SAR     is new Get_Discrete(SAR_Type);
   function Get_Wake    is new Get_Discrete(Wake_Type);
   function Get_Quantity is new Get_Discrete(Quantity);

   -- PROTOTYPES OF MORE SPECIFIC FUNCTIONS
   function Get_Waypoint(Root : in DOM_Element) return Waypoint;
   function Get_Waypoint(Root : in DOM_Element) return Airport;
   function Get_Cog_Limits(Root : in DOM_Element)
        return Cog_Limits_Lists.Vector;
   function Get_Equipment(Root : in DOM_Element) return Equipment;
   function Get_SAR_Equipment(Root : in DOM_Element) return SAR_Equipment;
   function Get_SAR_Data(Root : in DOM_Element) return SAR_Data;
   function Get_Weight_Limits(Root : in DOM_Element) return Weight_Limits;
   function Get_Fuel_Consumption(Root : in DOM_Element)
        return Fuel_Consumption_Record;
   function Get_Air_Speed(Root : in DOM_Element) return Air_Speed_Record;
   function Get_Time(Root : in DOM_Element) return Time;
   function Get_Wind(Root : in DOM_Element) return Wind;

   -- SEARCH FOR AND RETRIEVE ONE OF THE ABOVE TYPES
   function Get_Element is new Get_Single_Value(Change_Type,Get_Change_Type);
   function Get_Element is new Get_Single_Value(Boolean,Get_Boolean);
   function Get_Element is new Get_Single_Value(SAR_Data,Get_SAR_Data);
   function Get_Element is new Get_Single_Value(Minutes,Get_Minutes);
   function Get_Element is new Get_Single_Value(Seat_Content,Get_Seat_Content);
   function Get_Element is new Get_Single_Value(Unbounded_String,Get_String);
   function Get_Element is new Get_Single_Value(SSR_Type,Get_SSR);
   function Get_Element is new Get_Single_Value(Quantity,Get_Quantity);

   function Get_Element is new Get_Required_Single_Value(Air_Speed_Record,
                   Get_Air_Speed);
   function Get_Element is new Get_Required_Single_Value(Alt_Feet,Get_Alt);
   function Get_Element is new Get_Required_Single_Value(Arm,Get_Arm);
   function Get_Element is new Get_Required_Single_Value(
                   Cog_Limits_Lists.Vector, Get_Cog_Limits);
   function Get_Element is new Get_Single_Value(Degrees,Get_Degrees);
   function Get_Element is new Get_Required_Single_Value(
                   Equipment,Get_Equipment);
   function Get_Element is new Get_Required_Single_Value(
                   Fuel_Consumption_Record,Get_Fuel_Consumption);
   function Get_Element is new Get_Required_Single_Value(Kgs,Get_Kgs);
   function Get_Element is new Get_Required_Single_Value(Knots,Get_Knots);
   function Get_Element is new Get_Required_Single_Value(Litres,Get_Litres);
   function Get_Element is new Get_Single_Value(N_Miles,Get_N_Miles);
   function Get_Element is new Get_Required_Single_Value(SAR_Type,Get_SAR);
   function Get_Element is new Get_Required_Single_Value(
                   SAR_Equipment,Get_SAR_Equipment);
   function Get_Element is new Get_Required_Single_Value(Seat_Type,Get_Seat);
   function Get_Element is new Get_Required_Single_Value(Time,Get_Time);
   function Get_Element is new Get_Required_Single_Value(
                   Unbounded_String,Get_String);
   function Get_Element is new Get_Required_Single_Value(Waypoint,Get_Waypoint);
   function Get_Element is new Get_Required_Single_Value(Airport,Get_Waypoint);
   function Get_Element is new Get_Required_Single_Value(Wake_Type,Get_Wake);
   function Get_Element is new Get_Required_Single_Value(
                   Weight_Limits,Get_Weight_Limits);
   function Get_Element is new Get_Required_Single_Value(Wind,Get_Wind);

   ---------------------------
   -- GET COMPLICATED TYPES --
   ---------------------------

   -- GET EQUIPMENT FROM THIS ELEMENT
   function Get_Equipment(Root : in DOM_Element) return Equipment is
   begin
      Return (
      DME   => Get_Element(Root, "DME",   optional, false),
      GNSS  => Get_Element(Root, "GNSS",  optional, false),
      INS   => Get_Element(Root, "INS",   optional, false),
      ILS   => Get_Element(Root, "ILS",   optional, false),
      RNP   => Get_Element(Root, "RNP",   optional, false),
      UHF   => Get_Element(Root, "UHF",   optional, false),
      ADF   => Get_Element(Root, "ADF",   optional, false),
      VOR   => Get_Element(Root, "VOR",   optional, false),
      HF    => Get_Element(Root, "HF",    optional, false),
      DLink => Get_Element(Root, "DLink", optional, false),
      TAC   => Get_Element(Root, "TAC",   optional, false),
      VHF   => Get_Element(Root, "VHF",   optional, false),
      GPS   => Get_Element(Root, "GPS",   optional, false),
      ADS   => Get_Element(Root, "ADS",   optional, false),
      SSR   => Get_Element(Root, "SSR",   optional, None)
      );
   end Get_Equipment;

   -- GET SAR EQUIPMENT
   function Get_SAR_Equipment(Root : in DOM_Element) return SAR_Equipment is
      Empty : Unbounded_String := To_Unbounded_String("");
   begin
      Return (
      Light     => Get_Element(Root, "Light",     optional, false),
      UHF       => Get_Element(Root, "UHF",       optional, false),
      VHF       => Get_Element(Root, "VHF",       optional, false),
      Floures   => Get_Element(Root, "Floures",   optional, false),
      First_Aid => Get_Element(Root, "First_Aid", optional, false),
      Rations   => Get_Element(Root, "Rations",   optional, false),
      Water     => Get_Element(Root, "Water",     optional, false),
      Dinghies  => Get_Element(Root, "Dinghies",  optional, 0),
      Dinghies_Capacity => Get_Element(Root, "Dinghies_Capacity", optional, 0),
      Dinghies_Colour   => Get_Element(Root, "Dinghies_Colour",optional,Empty),
      Dinghies_Covers   => Get_Element(Root, "Dinghies_Covers",optional,False)
      );
   end Get_SAR_Equipment;

   -- GET SAR Data

   function No_SAR return SAR_DATA is
      Empty : Unbounded_String := To_Unbounded_String("");
      Dummy_Time : Time := Time_Of(Year_Number'First,1,1,0.0);
      SD : SAR_Data :=
        (
        What => None,
        Latest => Dummy_Time,
        Where => Empty,
        Phone => Empty,
        Fax => Empty,
        Equipment => (
             Dinghies => 0,
             Dinghies_Capacity => 0,
             Dinghies_Colour => Empty,
             Dinghies_Covers => False,
             Others => False
           )
        );
   begin
        return SD;
   end No_SAR;

   function Get_SAR_Data(Root : in DOM_Element) return SAR_Data is
      What  : SAR_Type;
      Empty : Unbounded_String := To_Unbounded_String("");
      E : SAR_Equipment := (
        Dinghies => 0,
        Dinghies_Capacity => 0,
        Dinghies_Colour => Empty,
        Dinghies_Covers => False,
        Others => False
      );
   begin
      What := Get_Element(Root, "Type");

      if (What = None) then
         return No_SAR;
      else
         return (
            What      => What,
            Latest    => Get_Element(Root, "When"),
            Where     => Get_Element(Root, "Where"),
            Phone     => Get_Element(Root, "Phone"),
            Fax       => Get_Element(Root, "Fax",       optional, empty),
            Equipment => Get_Element(Root, "Equipment")
         );
      end if;
   end Get_SAR_Data;

   -- GET WEIGHT LIMITS FROM THIS ELEMENT
   function Get_Weight_Limits(Root : in DOM_Element) return Weight_Limits is
   begin
      return (
      Mtow => Get_Element(Root, "MTOW"),
      Mlw  => Get_Element(Root, "MLW"),
      Mzfw => Get_Element(Root, "MZFW")
      );
   end Get_Weight_Limits;

   -- GET FUEL CONSUMPTION
   function Get_Fuel_Consumption(Root : in DOM_Element)
        return Fuel_Consumption_Record is
   begin
      return (
      Climb   => Get_Element(Root, "Climb"),
      Cruise  => Get_Element(Root, "Cruise"),
      Descend => Get_Element(Root, "Descend"),
      Holding => Get_Element(Root, "Holding"),
      Taxi    => Get_Element(Root, "Taxi"),
      Awk     => Get_Element(Root, "AWK")
      );
   end Get_Fuel_Consumption;

   -- GET AIR SPEEDS
   function Get_Air_Speed(Root : in DOM_Element) return Air_Speed_Record is
   begin
      return (
      Climb   => Get_Element(Root, "Climb"),
      Cruise  => Get_Element(Root, "Cruise"),
      Descend => Get_Element(Root, "Descend")
      );
   end Get_Air_Speed;

   -- GET SEAT FROM THIS ELEMENT
   function Get_Seat(Root : in DOM_Element) return Seat is
   begin
      return (
            Description => Get_Element(Root, "Description"),
            The_Seat_Type => Get_Element(Root, "Type"),
            The_Arm => Get_Element(Root, "Arm"),
            Maximum => Get_Element(Root, "Maximum")
              );
   end Get_Seat;

   function Get_Seat_Array is new Get_Array(Seat,Seat_Array,Get_Seat);
   function Get_Seat_Elements is new Get_Single_Array(Seat,Seat_Array,
        Get_Seat_Array);

   function Get_Seat_State(Root : in DOM_Element) return Seat_State is
   begin
      return (
            The_Weight => Get_Element(Root, "Weight"),
            Content => Get_Element(Root, "Content", optional, Passenger)
              );
   end Get_Seat_State;

   function Get_Seat_State_Array is new Get_Array(Seat_State,
        Seat_State_Array,Get_Seat_State);
   function Get_Seat_State_Elements is new Get_Single_Array(Seat_State,
        Seat_State_Array,Get_Seat_State_Array);

   function Get_Seat_Change(Root : in DOM_Element) return Seat_Change is
   begin
      return (
            Change => Get_Element(Root, "Type", optional, Replace),
            The_Weight => Get_Element(Root, "Weight"),
            Content => Get_Element(Root, "Content", optional, Passenger)
              );
   end Get_Seat_Change;

   function Get_Seat_Change_Array is new Get_Array(Seat_Change,
        Seat_Change_Array,Get_Seat_Change);
   function Get_Seat_Change_Elements is new Get_Single_Array(Seat_Change,
        Seat_Change_Array,Get_Seat_Change_Array);

   -- GET FUEL TANK FROM THIS ELEMENT
   function Get_Fuel_Tank(Root : in DOM_Element) return Fuel_Tank is
   begin
      return (
            Description => Get_Element(Root, "Description"),
            The_Arm => Get_Element(Root, "Arm"),
            Maximum => Get_Element(Root, "Maximum")
              );
   end Get_Fuel_Tank;

   function Get_Fuel_Tank_Array is new Get_Array(Fuel_Tank,
        Fuel_Tank_Array,Get_Fuel_Tank);
   function Get_Fuel_Tank_Elements is new Get_Single_Array(Fuel_Tank,
        Fuel_Tank_Array,Get_Fuel_Tank_Array);

   function Get_Fuel_Tank_State(Root : in DOM_Element)
        return Fuel_Tank_State is
   begin
      return (
            Qty => Get_Element(Root, "Litres")
              );
   end Get_Fuel_Tank_State;

   function Get_Fuel_Tank_State_Array is new Get_Array(Fuel_Tank_State,
        Fuel_Tank_State_Array,Get_Fuel_Tank_State);
   function Get_Fuel_Tank_State_Elements is new Get_Single_Array(
        Fuel_Tank_State, Fuel_Tank_State_Array,Get_Fuel_Tank_State_Array);

   function Get_Fuel_Tank_Change(Root : in DOM_Element)
        return Fuel_Tank_Change is
   begin
      return (
            Change => Get_Element(Root, "Type", optional, Replace),
            Qty => Get_Element(Root, "Litres")
              );
   end Get_Fuel_Tank_Change;

   function Get_Fuel_Tank_Change_Array is new Get_Array(Fuel_Tank_Change,
        Fuel_Tank_Change_Array,Get_Fuel_Tank_Change);
   function Get_Fuel_Tank_Change_Elements is new Get_Single_Array(
        Fuel_Tank_Change, Fuel_Tank_Change_Array,Get_Fuel_Tank_Change_Array);

   -- GET COG LIMITS ENTRY
   function Get_Cog_Limits_Entry(Root : in DOM_Element)
        return Cog_Limits_Entry is
   begin
      return (
      The_Weight   => Get_Element(Root, "Weight"),
      The_Arm  => Get_Element(Root, "Arm")
      );
   end Get_Cog_Limits_Entry;


   function Get_Cog_Limits(Root : in DOM_Element)
        return Cog_Limits_Lists.Vector is
      Name : DOM_String := "Entry";
      List : Cog_Limits_Lists.Vector;
      My_Nodes : Node_List := Child_Nodes(Root);
   begin
      for i in 0 .. Length(My_Nodes)-1  loop
         declare
            My_Node : Node renames Item(My_Nodes,i);
         begin
            if My_Node.Node_Type = Element_Node then
               if Node_Name(My_Node) = Name then
                  Append(List, Get_Cog_Limits_Entry(My_Node));
               end if;
            end if;
         end;
      end loop;
      return List;
   end Get_Cog_Limits;

   function Get_Waypoint(Root : in DOM_Element) return Waypoint is
      Name : String := Get_String(Root);
   begin
        return Get_Waypoint(Name);
   exception
   when No_Such_Waypoint =>
         Raise_Exception(Invalid_XML'Identity,"Waypoint "&Name&" not found.");
   end Get_Waypoint;

   function Get_Waypoint(Root : in DOM_Element) return Airport is
      Name : String := Get_String(Root);
   begin
      return Get_Waypoint(Name);
   exception
   when No_Such_Waypoint =>
      Raise_Exception(Invalid_XML'Identity,"Airport "&Name&" not found.");
   end Get_Waypoint;

   function Get_Time(Root : in DOM_Element) return Time is
      subtype Hour_Number is Integer range 0..23;
      subtype Minute_Number is Integer range 0..59;
      subtype Second_Number is Integer range 0..59;

      Value : String := Get_String(Root);

      Year : Year_Number := Year_Number'First;
      Month : Month_Number := 1;
      Day : Day_Number := 1;
      Hour : Hour_Number := 0;
      Minute : Minute_Number := 0;
      Second : Second_Number := 0;

      Left, Right : Natural := Value'First;
   begin

        Right := Index(Value(Left..Value'Last),"-") - 1;
--      Put_Line("Year: "&Value(Left..Right));
        Year := Year_Number'Value(Value(Left..Right));

        Left := Right + 2;
        Right := Index(Value(Left..Value'Last),"-") - 1;
--      Put_Line("Month: "&Value(Left..Right));
        Month := Month_Number'Value(Value(Left..Right));

        Left := Right + 2;
        Right := Index(Value(Left..Value'Last)," ") - 1;
--      Put_Line("Day: "&Value(Left..Right));
        Day := Day_Number'Value(Value(Left..Right));

        Left := Right + 2;
        Right := Index(Value(Left..Value'Last),":") - 1;
--      Put_Line("Day: "&Value(Left..Right));
        Hour := Hour_Number'Value(Value(Left..Right));

        Left := Right + 2;
        Right := Index(Value(Left..Value'Last),":") - 1;
--      Put_Line("Minute: "&Value(Left..Right));
        Minute := Minute_Number'Value(Value(Left..Right));

        Left := Right + 2;
        Right := Value'Last;
--      Put_Line("Second: "&Value(Left..Right));
        Second := Second_Number'Value(Value(Left..Right));

        return Time_Of(Year,Month,Day,Duration((Hour*60+Minute)*60+Second));
   exception
   when Constraint_Error =>
         Raise_Exception(Invalid_XML'Identity,"Time "&Value&" is invalid.");
   end Get_Time;

   function Get_Wind(Root : in DOM_Element) return Wind is
      Value : String := Get_String(Root);

      Direction : Degrees := 0;
      Speed : Knots := 0;

      Left, Right : Natural := Value'First;

      The_Wind : Wind;

   begin
        Right := Index(Value(Left..Value'Last),"/") - 1;
--      Put_Line("Direction: "&Value(Left..Right));
        Direction := Degrees'Value(Value(Left..Right));

        Left := Right + 2;
        Right := Value'Last;
--      Put_Line("Speed: "&Value(Left..Right));
        Speed := Knots'Value(Value(Left..Right));

        Initialize(The_Wind,Direction,Speed);
        return The_Wind;
   exception
   when Constraint_Error =>
         Raise_Exception(Invalid_XML'Identity,"Wind "&Value&" is invalid.");
   end Get_Wind;


   function Get_Leg(Root : in DOM_Element; V : Maps.Vector; Last_Leg : Boolean)
        return Leg_Access is
      The_Leg : Leg_Access := new Leg;
      Empty : Unbounded_String := To_Unbounded_String("");
   begin
      Initialize(The_Leg.all,
              Lowest_Safe_Alt => Get_Element(Root, "Lowest_Safe_Altitude"),
              Alt => Get_Element(Root, "Altitude"),
              Track => Get_Element(Root, "Track", optional, V.Mag_Direction),
              Distance => Get_Element(Root, "Distance", optional, V.Distance),
              Awk_Time => Get_Element(Root, "AWK_Time", optional, 0),
              Awk_Rmk => Get_Element(Root, "AWK_Remark", optional, Empty),
              Holding_Time => Get_Element(Root, "Holding_Time", optional, 0),
              Taxi_Time => Get_Element(Root, "Taxi_Time", optional, 0)
        );

        if not Last_Leg then
                Set_Destination(The_Leg.all,
                        Get_Element(Root, "Destination"));
                Set_Alt_End(The_Leg.all,
                        Get_Element(Root, "Destination_Altitude"));
        end if;

      Set_Wind(The_Leg.all,Get_Element(Root, "Wind"));

      return(The_Leg);
   end Get_Leg;

   function Get_Flight(Root : in DOM_Element)
        return Flight_Access is
      The_Flight : Flight_Access := new Flight;

      Name : DOM_String := "Leg";
      Name_Alt : DOM_String := "Alternate";
      My_Nodes : Node_List := Child_Nodes(Root);

      Seat_List : Seat_Change_Array :=
         Get_Seat_Change_Elements(Root, "Load","Seat",0);
      Fuel_Tank_List : Fuel_Tank_Change_Array :=
         Get_Fuel_Tank_Change_Elements(Root, "Load","Fuel_Tank",0);

      Change : Aircraft_Change := (
         Seats => Seat_List'Length,
         Seat_List => Seat_List,
         Fuel_Tanks => Fuel_Tank_List'Length,
         Fuel_Tank_List => Fuel_Tank_List
      );

      Takeoff, Landing, Alternate : Airport;
      Departure_Time : Time;
      W1, W2 : Waypoint;
      M : Maps.Map;
      V : Maps.Vector;

      Count : Natural := Get_Count_Of_Elements(Root,Name);

   begin
      Takeoff := Get_Element(Root, "Takeoff");
      Departure_Time := Get_Element(Root,"Departure_Time");
      Set_Takeoff(The_Flight.all, Departure_Time, Takeoff);

      Set_Takeoff_Change(The_Flight.all,Change);

      Landing := Get_Element(Root, "Landing");
      Set_Landing(The_Flight.all, Landing);

      W1 := Waypoint(Takeoff);
      for i in 0 .. Length(My_Nodes)-1  loop
         declare
            My_Node : Node renames Item(My_Nodes,i);
            Last_Leg : Boolean;
         begin
            if My_Node.Node_Type = Element_Node then
               if Node_Name(My_Node) = Name then
                  if Count > 0 then
                        Count := Count - 1;
                  end if;
                  Last_Leg := (Count = 0);

                  if Last_Leg then
                        W2 := Waypoint(Landing);
                  else
                        W2 := Get_Element(My_Node, "Destination");
                  end if;
                  V := Maps.Get_Distance(M, W1.Pos, W2.Pos);
                  Add_Leg(The_Flight.all, Get_Leg(My_Node, V, Last_Leg));
                  W1 := W2;
               elsif Node_Name(My_Node) = Name_Alt then
                  Alternate := Get_Element(My_Node, "Destination");
                  W2 := Waypoint(Alternate);
                  V := Maps.Get_Distance(M, W1.Pos, W2.Pos);
                  Set_Alternate_Leg(The_Flight.all,
                     Get_Leg(My_Node, V, Last_Leg => True),Alternate);
                  W1 := W2;
               end if;
            end if;
         end;
      end loop;

      return The_Flight;
   end Get_Flight;

   function Get_Flightplan(Root : in DOM_Element)
        return Flightplan_Access is
      The_Flightplan : Flightplan_Access := new Flightplan;

      Name : DOM_String := "Flight";
      My_Nodes : Node_List := Child_Nodes(Root);

      SAR : Sar_Data := Get_Element(Root, "Sar_Data", optional, No_SAR);
   begin

      Set_SAR_Data(The_Flightplan.all, SAR);

      for i in 0 .. Length(My_Nodes)-1  loop
         declare
            My_Node : Node renames Item(My_Nodes,i);
         begin
            if My_Node.Node_Type = Element_Node then
               if Node_Name(My_Node) = Name then
                  Add_Flight(The_Flightplan.all, Get_Flight(My_Node));
               end if;
            end if;
         end;
      end loop;
      return The_Flightplan;
   end Get_Flightplan;

   -- GET AIRCRAFT DATA
   function Get_Aircraft_Data(Root : in DOM_Element) return Aircraft_Data is
      Seats : Seat_Array := Get_Seat_Elements(Root,"Loading","Seat",1);
      Fuel_Tanks : Fuel_Tank_Array := Get_Fuel_Tank_Elements(Root,"Loading","Fuel_Tank",1);
   begin
      if Seats'Length = 0 then
         Raise_Exception(Invalid_XML'Identity,"No Seats defined");
      end if;
      if Fuel_Tanks'Length = 0 then
         Raise_Exception(Invalid_XML'Identity,"No Fuel Tanks defined");
      end if;
      return (
              Callsign => Get_Element(Root, "Callsign"),
              Full_Callsign => Get_Element(Root, "Full_Callsign"),
              The_Type => Get_Element(Root, "Type"),
              Approved_Type => Get_Element(Root, "Approved_Type",
                      optional, False),
              Colour => Get_Element(Root, "Colour"),
              Description => Get_Element(Root, "Description"),
              The_Equipment => Get_Element(Root, "Equipment"),
              The_Wake_Type => Get_Element(Root, "Wake"),
              Fuel_Consumption => Get_Element(Root, "Fuel_Consumption"),
              Air_Speed => Get_Element(Root, "Air_Speed"),
              The_Weight_Limits => Get_Element(Root, "Limits"),
              Fuel_Tanks => Fuel_Tanks'Length,
              Fuel_Tank_List => Fuel_Tanks,
              Seats => Seats'Length,
              Seat_List => Seats,
              Cog_Limits => Get_Element(Root, "COG"),
              Bew => Get_Element(Root, "Basic_Empty_Weight"),
              Bew_Arm => Get_Element(Root, "Arm")
              );
      end;

   --
   function Parse_File(Filename : string; Name : DOM_String)
           return DOM_Element is
      Read : File_Input;
      My_Tree_Reader : Tree_Reader;

      My_Tree : Document;
      My_Root : DOM_Element;

   begin
      Set_Public_Id (Read, Filename);
      Set_System_Id (Read, Filename);
      Open (Filename, Read);

      Set_Feature (My_Tree_Reader, Validation_Feature, True);

      Parse (My_Tree_Reader, Read);
      Close (Read);

      Normalize(Get_Tree(My_Tree_Reader));

      My_Tree := Get_Tree(My_Tree_Reader);
      My_Root := Get_Element(My_Tree);

      if Node_Name(My_Root) /= Name then
         Raise_Exception(Invalid_XML'Identity,"The element "&Name&
                         " was expected but "&Node_Name(My_Root)&
                         " was found instead");
      end if;

      return My_Root;
   end Parse_File;

   function Get_Equipment(Filename : string) return Equipment is
      My_Root : DOM_Element;
   begin
      My_Root := Parse_File(Filename,"Equipment");
      return Get_Equipment(My_Root);
   end Get_Equipment;

   function Get_Aircraft_Data(Filename : string) return Aircraft_Data is
      My_Root : DOM_Element;
   begin
      My_Root := Parse_File(Filename,"Aircraft");
      return Get_Aircraft_Data(My_Root);
   end Get_Aircraft_Data;

   function Get_Leg(Filename : string) return Leg_Access is
      My_Root : DOM_Element;
   begin
      My_Root := Parse_File(Filename,"Leg");
      return Get_Leg(My_Root,(0,0,0),Last_Leg => false);
   end Get_Leg;

   function Get_Flight(Filename : string) return Flight_Access is
      My_Root : DOM_Element;
   begin
      My_Root := Parse_File(Filename,"Flight");
      return Get_Flight(My_Root);
   end Get_Flight;

   function Get_Flightplan(Filename : string) return Flightplan_Access is
      My_Root : DOM_Element;
   begin
      My_Root := Parse_File(Filename,"Flightplan");
      return Get_Flightplan(My_Root);
   end Get_Flightplan;

end Xml;
