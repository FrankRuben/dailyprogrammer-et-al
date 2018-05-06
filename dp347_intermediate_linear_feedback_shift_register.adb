with Ada.Command_Line;
with Ada.Strings.Bounded;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Text_IO;
with GNAT.Spitbol.Patterns;

use type GNAT.Spitbol.Patterns.Pattern;

procedure Dp347_Intermediate_Linear_Feedback_Shift_Register is

   package Cli renames Ada.Command_Line;
   package Pat renames GNAT.Spitbol.Patterns;
   package Tio renames Ada.Text_IO;
   package Ustr renames Ada.Strings.Unbounded;

   generic
      type Reg_Value_Type is mod <>;    -- we encode the seed and the register state with this type
      type Reg_Size_Type is range <>;   -- the size of the register is within this range
      Reg_Size : in Reg_Size_Type;      -- the concrete size of the register, a runtime parameter
   package Lsfr is
      procedure Init
        (Arg_Tap_Pos         : Reg_Value_Type;
         Arg_Seed            : Reg_Value_Type;
         Arg_Invert_Feedback : Boolean);
      procedure Run (Nb_Clock_Steps : Natural);

   private
      function To_Binary_String (Num : Reg_Value_Type) return String;

      Output_Bit : constant Reg_Value_Type := (2**Natural (Reg_Size - 1));
      All_Bits   : constant Reg_Value_Type := (2**Natural (Reg_Size)) - 1;

      Tap_Pos         : Reg_Value_Type; -- tap positions as 0-based bits, so 0 is leftmost bit
      Seed            : Reg_Value_Type; -- seed value as 0-based bits, so 0 is leftmost bit
      Invert_Feedback : Boolean;        -- True for XNOR, False for XOR
   end Lsfr;

   package body Lsfr is
      procedure Init
        (Arg_Tap_Pos         : Reg_Value_Type;
         Arg_Seed            : Reg_Value_Type;
         Arg_Invert_Feedback : Boolean)
      is
      begin
         Tap_Pos         := Arg_Tap_Pos;
         Seed            := Arg_Seed;
         Invert_Feedback := Arg_Invert_Feedback;
      end Init;

      procedure Run (Nb_Clock_Steps : Natural) is
         State : Reg_Value_Type := Seed;
      begin
         for Clock_Step in Integer range 1 .. Nb_Clock_Steps loop
            declare
               Tap_Bits   : Reg_Value_Type := Tap_Pos;
               State_Bits : Reg_Value_Type := State;
               Is_First   : Boolean        := True;
               Res        : Boolean;
            begin
               loop
                  if (Tap_Bits and 1) /= 0 then
                     if Is_First then
                        Res      := (State_Bits and 1) /= 0;
                        Is_First := False;
                     else
                        Res := Res xor ((State_Bits and 1) /= 0);
                        if Invert_Feedback then
                           Res := not Res;
                        end if;
                     end if;
                  end if;
                  Tap_Bits := Tap_Bits / 2;
                  exit when Tap_Bits = 0;
                  State_Bits := State_Bits / 2;
               end loop;

               State := (State * 2) and All_Bits;
               if Res then
                  State := State or 1;
               end if;

               Tio.Put_Line (Natural'Image (Clock_Step) & " " & To_Binary_String (State));
            end;
         end loop;
      end Run;

      function To_Binary_String (Num : Reg_Value_Type) return String is
         package Bstr is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => Integer (Reg_Size));
         use type Bstr.Bounded_String;
         Res  : Bstr.Bounded_String;
         Bits : Reg_Value_Type := Num;
      begin
         for I in Reg_Size_Type range 1 .. Reg_Size loop
            Res  := Res & (if (Bits and 1) /= 0 then '1' else '0');
            Bits := Bits / 2;
         end loop;
         return Bstr.To_String (Res);
      end To_Binary_String;
   end Lsfr;

   procedure Process_Line (Line_Str : in String) is

      Max_Reg_Size : constant Natural := 32;
      type Reg_Value_Type is mod 2**Max_Reg_Size;
      type Reg_Size_Type is range 1 .. Max_Reg_Size;

      function Trim_Balanced
        (Vstr : GNAT.Spitbol.VString) return String is
        (GNAT.Spitbol.S (GNAT.Spitbol.Substr (Vstr, 2, GNAT.Spitbol.Size (Vstr) - 2)));

      function Slice_Token
        (Str                     : String;
         Token_Start, Token_Stop : Integer) return String is
        (Ustr.Slice (Ustr.To_Unbounded_String (Str), Token_Start, Token_Stop));

      function Parse_Tap_Pos (Str : String) return Reg_Value_Type is
         Tap_Pos                 : Reg_Value_Type := 0;
         Lstr_Start              : Integer        := Str'First;
         Token_Start, Token_Stop : Integer;
      begin
         for I in Integer range 0 .. 31 loop
            Ada.Strings.Fixed.Find_Token
              (Str,
               Ada.Strings.Maps.To_Set (','),
               Lstr_Start,
               Ada.Strings.Outside,
               Token_Start,
               Token_Stop);
            exit when Token_Stop = 0;
            declare
               Token_Str : constant String  := Slice_Token (Str, Token_Start, Token_Stop);
               Token_Pos : constant Integer := Integer'Value (Token_Str);
            begin
               Tap_Pos := Tap_Pos or (2**Token_Pos);
            end;
            Lstr_Start := Token_Stop + 1;
         end loop;
         return Tap_Pos;
      end Parse_Tap_Pos;

      function Reverse_String (Str : String) return String is
         Result : String (Str'Range);
      begin
         for I in Str'Range loop
            Result (Result'Last - I + Str'First) := Str (I);
         end loop;
         return Result;
      end Reverse_String;

      function Parse_Seed
        (Str : String) return Reg_Value_Type is
        (Reg_Value_Type'Value ("2#" & Str & '#'));

      Match_Tap_Pos_Seq    : GNAT.Spitbol.VString := GNAT.Spitbol.Nul;
      Match_Fname          : GNAT.Spitbol.VString := GNAT.Spitbol.Nul;
      Match_Seed_Bits      : GNAT.Spitbol.VString := GNAT.Spitbol.Nul;
      Match_Nb_Clock_Steps : GNAT.Spitbol.VString := GNAT.Spitbol.Nul;

      Pattern_Ws     : constant Pat.Pattern := Pat.Span (' ' & ASCII.HT);
      Pattern_Opt_Ws : constant Pat.Pattern := Pat.NSpan (' ' & ASCII.HT);
      Pattern_Bits   : constant Pat.Pattern := Pat.Span ("01");
      Pattern_Int : constant Pat.Pattern := Pat.Span (Ada.Strings.Maps.Constants.Decimal_Digit_Set);
      Pattern_Line   : constant Pat.Pattern :=
        Pattern_Opt_Ws &
        Pat.Bal * Match_Tap_Pos_Seq &
        Pattern_Ws &  -- match balanced parenthesis
        ("XOR" or "XNOR") * Match_Fname &
        Pattern_Ws & -- match register function
        Pattern_Bits * Match_Seed_Bits &
        Pattern_Ws &
        Pattern_Int * Match_Nb_Clock_Steps;

   begin
      Pat.Anchored_Mode := True;
      if Pat.Match (Line_Str, Pattern_Line) then
         declare
            package This_Lsrf is new Lsfr
              (Reg_Value_Type => Reg_Value_Type,
               Reg_Size_Type  => Reg_Size_Type,
               Reg_Size       => Reg_Size_Type (GNAT.Spitbol.Size (Match_Seed_Bits)));
         begin
            This_Lsrf.Init
              (Parse_Tap_Pos (Trim_Balanced (Match_Tap_Pos_Seq)),
               Parse_Seed (Reverse_String (GNAT.Spitbol.S (Match_Seed_Bits))),
               Ada.Strings.Equal_Case_Insensitive (GNAT.Spitbol.S (Match_Fname), "XNOR"));
            This_Lsrf.Run (Natural'Value (GNAT.Spitbol.S (Match_Nb_Clock_Steps)));
         end;
      else
         Tio.Put_Line ("Cannot parse " & Line_Str);
      end if;
   end Process_Line;

begin
   for I in 1 .. Cli.Argument_Count loop
      Process_Line (Cli.Argument (I));
   end loop;
end Dp347_Intermediate_Linear_Feedback_Shift_Register;

-- See here: https://www.reddit.com/r/dailyprogrammer/comments/7r17qr/20180117_challenge_347_intermediate_linear/

-- Run this using:
-- gnatmake dp347_intermediate_linear_feedback_shift_register.adb && ./dp347_intermediate_linear_feedback_shift_register "[0,2] XOR 001 7" "[0,2] XNOR 001 7" "[1,2,3,7] XOR 00000001 16" "[1,5,6,31] XOR 00000000000000000000000000000001 16"
