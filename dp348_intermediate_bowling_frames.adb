with Ada.Command_Line;
with Ada.Strings;
with Ada.Text_IO;

procedure Dp348_Intermediate_Bowling is

   package Cli renames Ada.Command_Line;
   package Tio renames Ada.Text_IO;

   subtype Nb_Attempts_Type is Natural range 0 .. 2; -- we reset to 0, then throw twice
   subtype Nb_Pins_Type is Natural range 0 .. 10;    -- we can have attemmpts with 0 pins
   package Nio is new Tio.Integer_IO (Nb_Pins_Type);

   procedure Put_Frame (Line_Str : String) is
      Nb_Attempts   : Nb_Attempts_Type := 0;
      Nb_Frame_Pins : Nb_Pins_Type     := 0;
      Nb_Roll_Pins  : Nb_Pins_Type;

      procedure Put_Attempt is
      begin
         Nb_Attempts := Nb_Attempts + 1;
         if Nb_Attempts = 1 and Nb_Roll_Pins = 10 then
            Tio.Put ("X ");
            Nb_Frame_Pins := 0;
            Nb_Attempts   := 0;
         else
            Nb_Frame_Pins := Nb_Frame_Pins + Nb_Roll_Pins;
            if Nb_Frame_Pins = 10 then
               Tio.Put ('/');
            elsif Nb_Roll_Pins = 0 then
               Tio.Put ('-');
            else
               Nio.Put (Nb_Roll_Pins, Width => 1);
            end if;
            if Nb_Attempts = 2 then
               Nb_Frame_Pins := 0;
               Nb_Attempts   := 0;
               Tio.Put (' ');
            end if;
         end if;
      end Put_Attempt;

      First_Str_Pos : Natural := 1;
      Last_Str_Pos  : Positive;

   begin
      loop
         Nio.Get (Line_Str (First_Str_Pos .. Line_Str'Last), Nb_Roll_Pins, Last_Str_Pos);
         Put_Attempt;
         exit when Last_Str_Pos = Line_Str'Last;
         First_Str_Pos := Last_Str_Pos + 1;
      end loop;
   end Put_Frame;

begin
   for I in 1 .. Cli.Argument_Count loop
      Put_Frame (Cli.Argument (I));
   end loop;
end Dp348_Intermediate_Bowling;

-- See: https://www.reddit.com/r/dailyprogrammer/comments/7so37o/20180124_challenge_348_intermediate_bowling/

-- Run this using:
-- gnatmake dp348_intermediate_bowling_frames.adb && ./dp348_intermediate_bowling_frames "6 4 5 3 10 10 8 1 8 0 10 6 3 7 3 5 3"
-- gnatmake dp348_intermediate_bowling_frames.adb && ./dp348_intermediate_bowling_frames "9  0  9  0  9  0  9  0  9  0  9  0  9  0  9  0  9  0  9  0"
-- gnatmake dp348_intermediate_bowling_frames.adb && ./dp348_intermediate_bowling_frames "10 10 10 10 10 10 10 10 10 10 10 10"
-- gnatmake dp348_intermediate_bowling_frames.adb && ./dp348_intermediate_bowling_frames "5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5"
-- gnatmake dp348_intermediate_bowling_frames.adb && ./dp348_intermediate_bowling_frames "10 3  7  6  1  10 10 10 2  8  9  0  7  3  10 10
-- gnatmake dp348_intermediate_bowling_frames.adb && ./dp348_intermediate_bowling_frames "9  0  3  7  6  1  3  7  8  1  5  5  0  10 8  0  7  3  8  2  8"
