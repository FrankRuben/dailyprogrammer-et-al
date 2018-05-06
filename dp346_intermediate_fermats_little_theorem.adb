with Ada.Command_line;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings; with Ada.Strings.Unbounded;
with Ada.Text_Io;
with Interfaces.C;
with GNATCOLL.GMP.Random_State; with GNATCOLL.GMP.Integers.Random;
with GNATCOLL.GMP; with GNATCOLL.GMP.Integers;

procedure Dp346_Intermediate_Fermat is

   package Tio renames Ada.Text_IO;
   package Fstr renames Ada.Strings.Fixed;
   package Ustr renames Ada.Strings.Unbounded;
   package Cli renames Ada.Command_Line;
   package Gmp_Int renames GNATCOLL.GMP.Integers;
   package Gmp_Rnd_State renames GNATCOLL.GMP.Random_State;

   type Certainty_Type is digits 4 range 0.000 .. 1.000;

   Rnd_State : Gmp_Rnd_State.Generator;

   procedure Mod_Exp (Base : in Gmp_Int.Big_Integer; Exp : in Gmp_Int.Big_Integer;
                      Modulus : in Gmp_Int.Big_Integer; Res : out Gmp_Int.Big_Integer)
   with Post => Gmp_Int."<" (Res, Modulus);

   procedure Mod_Exp (Base : in Gmp_Int.Big_Integer; Exp : in Gmp_Int.Big_Integer;
                      Modulus : in Gmp_Int.Big_Integer; Res : out Gmp_Int.Big_Integer)
     -- https://en.wikipedia.org/wiki/Modular_exponentiation; Right-to-left binary method
   is Lbase : Gmp_Int.Big_Integer;
      Lexp : Gmp_Int.Big_Integer;

      function Big_Int_Non_Zero (Bi : Gmp_Int.Big_Integer) return Boolean
        is (Gmp_Int.">" (Bi, 0));

      function Big_Int_Odd (Bi : Gmp_Int.Big_Integer) return Boolean
        is (not Gmp_Int."=" (Gmp_Int."mod" (Bi, 2), 0));

   begin
      if Gmp_Int."=" (Modulus, 1) then
         Gmp_Int.Set (Res, 0);
      else
         Gmp_Int.Set (Res, 1);
         Gmp_Int.Set (Lexp, Exp);
         Gmp_Int.Get_Mod (Lbase, Base, Modulus);
         while Big_Int_Non_Zero (Lexp) loop
            if Big_Int_Odd (Lexp) then
               Gmp_Int.Get_Mod (Res, Gmp_Int."*" (Res, Lbase), Modulus);
            end if;
            Gmp_Int.Divide (Lexp, Lexp, 2);
            Gmp_Int.Multiply (Lbase, Lbase);
            Gmp_Int.Get_Mod (Lbase, Lbase, Modulus);
         end loop;
      end if;
   end;

   function Is_Prime_With_Certainty (Number_To_Test : Gmp_Int.Big_Integer; Req_Certainty : Certainty_Type)
      return Boolean
   is Curr_Uncertainty : Certainty_Type := 0.5;

      function Is_No_Prime (Number_To_Test : Gmp_Int.Big_Integer)
         return Boolean
      is Rnd : constant Gmp_Int.Big_Integer := Gmp_Int.Random.Number (Rnd_State, Number_To_Test);
         Res : Gmp_Int.Big_Integer;
      begin
         Mod_Exp (Rnd, Number_To_Test, Number_To_Test, Res);
         return not Gmp_Int."=" (Rnd, Res);
      end;

   begin
      loop
         if Is_No_Prime (Number_To_Test) then return False; end if;
         exit when 1.0 - Curr_Uncertainty >= Req_Certainty;
         Curr_Uncertainty := Curr_Uncertainty / 2.0;
      end loop;
      return True;
   end;

   procedure Process_File (File_Name : String) is

      function Next_Token (In_Str : in String; Token_Stop : out Integer;
                           Str_Start : in Integer := -1; Str_End : in Integer := -1)
         return String
           -- We only allow to call that when we know that a token will be found, otherwise
           -- Find_Token returns 0 for Token_Stop, if no next token exists:
      with Post => Token_Stop > 0 and Next_Token'Result'Length > 0;

      function Next_Token (In_Str : in String; Token_Stop : out Integer;
                           Str_Start : in Integer := -1; Str_End : in Integer := -1)
         return String
      is Spaces_Set : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (Ada.Strings.Space);
         Lstr_End : constant Integer := (if Str_End < 0 then In_Str'Last else Str_End);
         Lstr_Start : constant Integer := (if Str_Start < 0 then In_Str'First else Str_Start);
         Token_Start : Integer;
      begin
         Fstr.Find_Token (In_Str, Spaces_Set, Lstr_Start, Ada.Strings.Outside, Token_Start, Token_Stop);
         if Token_Stop > Lstr_End then Token_Stop := Lstr_End; end if;
         return Ustr.slice (Ustr.To_Unbounded_String (In_Str), Token_Start, Token_Stop);
      end Next_Token;

      function Parse_Line (Line_Str : in String; Req_Certainty : out Certainty_Type)
         return Gmp_Int.Big_Integer
      is
         Num_Stop, Certainty_Stop : Integer;
         Num_Str : constant String := Next_Token (Line_Str, Num_Stop);
         Certainty_Start : constant Integer := Num_Stop + 1;
         Certainty_Str : constant String := Next_Token (Line_Str, Certainty_Stop, Certainty_Start);
      begin
         Req_Certainty := Certainty_Type'Value (Certainty_Str);
         return Gmp_Int.Make (Num_Str);
      end;

      File : Tio.File_Type;
   begin
      Tio.Open (File => File, Mode => Tio.In_File, Name => File_Name);
      while not Tio.End_OF_File (File) loop
         declare
            Line_Str : constant String := Tio.Get_Line(File);
            Req_Certainty : Certainty_Type;
            Number_To_Test : constant Gmp_Int.Big_Integer := Parse_Line (Line_Str, Req_Certainty);
            Is_Prime : constant Boolean := Is_Prime_With_Certainty (Number_To_Test, Req_Certainty);
            Res_Str : constant String := (if Is_Prime then "True" else "False");
         begin
            Tio.Put_Line (Gmp_Int.Image (Number_To_Test) & ", " & Certainty_Type'Image (Req_Certainty)
                            & " -> " & Res_Str);
         end;
      end loop;
      Tio.Close (File);
   end Process_File;

begin
   Gmp_Rnd_State.Initialize (Rnd_State);
   Process_File ("dp346_intermediate_fermat_challenge.txt");
end Dp346_Intermediate_Fermat;

-- See here: https://www.reddit.com/r/dailyprogrammer/comments/7pmt9c/20180110_challenge_346_intermediate_fermats/

-- Run like this (no bonus):
-- gnatmake dp346_intermediate_fermats_little_theorem.adb -aI/usr/share/ada/adainclude/gnatcoll -aI/usr/share/ada/adainclude/gnatcoll_gmp/ -largs -L/usr/lib/x86_64-linux-gnu/ -lgmp -cargs -gnata && ./dp346_intermediate_fermats_little_theorem
