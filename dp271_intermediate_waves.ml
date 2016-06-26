module CharMap = Map.Make (Char)

let write_wav_header out_channel data_len sample_rate =
  let output_i16 out_channel n =
    output_char out_channel (char_of_int ((n lsr  0) land 0xff));
    output_char out_channel (char_of_int ((n lsr  8) land 0xff)) in

  let output_i32 out_channel n =
    output_i16 out_channel ((n lsr  0) land 0xffff);
    output_i16 out_channel ((n lsr 16) land 0xffff) in

  let bits_per_note = 8 in              (* fixed because of `note_byte' below *)
  let num_channels = 1 in
  let header_len = 44 in

  output_string out_channel "RIFF";
  output_i32 out_channel (data_len + header_len - 8); (* size of overall file *)
  output_string out_channel "WAVE";
  output_string out_channel "fmt ";
  output_i32 out_channel 16;            (* length of format data as listed above *)
  output_i16 out_channel 1;             (* 1: PCM *)
  output_i16 out_channel num_channels;
  output_i32 out_channel sample_rate;
  output_i32 out_channel (sample_rate * bits_per_note * num_channels / 8);
  output_i16 out_channel (bits_per_note * num_channels / 8);
  output_i16 out_channel bits_per_note;
  output_string out_channel "data";
  output_i32 out_channel data_len

let write_note out_channel sample_rate bytes_per_note note_freq =
  let output_note out_channel n =
    let note_byte f = 128 + int_of_float (127.0 *. f) in
    output_byte out_channel (note_byte n) in (* `output_byte' writes the given integer taking its modulo 256 *)

  let pi = 4.0 *. atan 1.0 in
  let note_sin i = sin(2.0 *. pi *. note_freq *. float_of_int i /. float_of_int sample_rate) in
  for i = 0 to bytes_per_note - 1 do
    output_note out_channel (note_sin i);
  done;;

let () =
  let freq_map =
    List.fold_left
      (fun map (key, value) -> CharMap.add key value map)
      CharMap.empty (* frequency in Hz *)
      [('A', 440.00); ('B', 493.88); ('C', 523.25); ('D', 587.33);
       ('E', 659.25); ('F', 698.46); ('G', 783.99); ('_', 0.00)] in

  let save_freq ch dflt_ch =
    try
      CharMap.find ch freq_map
    with Not_found ->
      CharMap.find dflt_ch freq_map in

  let write_notes out_channel sample_rate bytes_per_note notes =
    String.iter (fun ch -> write_note out_channel sample_rate bytes_per_note (save_freq ch '_')) notes in

  let sample_rate = 8000 in
  let duration_ms = 300 in
  let bytes_per_note = sample_rate * duration_ms / 1000 in
  let notes = "ABCDEFG_GFEDCBA" in

  set_binary_mode_out stdout true;
  if Array.length Sys.argv > 1 then begin
    let num_notes = String.length notes in
    write_wav_header stdout (num_notes * bytes_per_note) sample_rate;
    write_notes stdout sample_rate bytes_per_note notes
  end else
    write_notes stdout sample_rate bytes_per_note notes;
  flush stdout;;

(*
 * See https://www.reddit.com/r/dailyprogrammer/comments/4o74p3/20160615_challenge_271_intermediate_making_waves/
 *)
 
(*
 * Local Variables:
 * compile-command: "ocaml -safe-string dp271-medium-waves.ml | aplay -t raw && ocaml -safe-string dp271-medium-waves.ml wav | aplay -t wav"
 * End:
 *)
