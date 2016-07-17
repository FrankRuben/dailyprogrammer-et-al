let max_symbol_chars = 26
let nb_symbols = max_symbol_chars * max_symbol_chars

type element = string
type symbol = string
type element_idx = int

type element_table = element array

exception No_free_symbol_for_element of element

(* Compute integer value from the 2 symbol chars that can be used to index an `element_table' array. *)
let make_idx c0 c1 : element_idx =
  assert (c1 == Char.lowercase_ascii c1);
  let base = Char.code 'a' in
  let i0 = (Char.code @@ Char.lowercase_ascii c0) - base in
  let i1 = (Char.code c1) - base in
  let idx = i0 * max_symbol_chars + i1 in
  assert (i0 >= 0 && i0 < max_symbol_chars);
  assert (i1 >= 0 && i1 < max_symbol_chars);
  assert (idx >= 0 && idx < nb_symbols);
  idx

(* Return next free symbol (and `element_table' index) according to Splurthian's rules for element. *)
let find_symbol (elements : element_table) (element : element) : (element_idx * symbol) =
  let make_symbol c0 c1 =
    let symbol_chars = [| Char.uppercase_ascii c0; c1 |] in
    String.init 2 (fun i -> symbol_chars.(i))
  in
  let rec inner c0 from_idx to_idx : (element_idx option * char) =
    if from_idx = to_idx then (None, '_')
    else let c1 = element.[from_idx] in
         let idx = make_idx c0 c1 in
         match elements.(idx) with
         | "" -> (Some idx, c1)                (* symbol is still free, use it *)
         | _ -> inner c0 (from_idx + 1) to_idx (* symbol already reserved for another element, keep on searching *)
  in
  let rec outer from_idx to_idx : (element_idx option * char * char) =
    if from_idx = to_idx then (None, '_', '_')
    else let c0 = element.[from_idx] in
         match inner c0 (from_idx + 1) (to_idx + 1) with
         | None, _ -> outer (from_idx + 1) to_idx
         | Some idx, c1 -> (Some idx, c0, c1)
  in
  let element_len = String.length element in
  match outer 0 (element_len - 1) with
  | None, _, _ ->
     raise (No_free_symbol_for_element element)
  | Some idx, c0, c1 ->
     elements.(idx) <- element;         (* mark symbol as reserved for current element *)
     (idx, make_symbol c0 c1)           (* build symbol from free characters as found *)

(* Generate element table and compute its score using the given "optimization function" picking the table elements. *)
let eval_score skip_element_fn elements element_buf max_nb_symbols : (symbol list * int) =
  let rec score_loop score prev_symbol = function
    | [] -> score
    | curr_symbol :: _ when prev_symbol > curr_symbol -> score
    | curr_symbol :: rest_symbols -> score_loop (score + 1) curr_symbol rest_symbols
  in
  let get_score = function
    | [] -> 0
    | first_symbol :: rest_lines -> score_loop 1 first_symbol rest_lines
  in
  (* The elements of the candidates file are already sorted in alphabetical order. So when inserting symbols *)
  (* here in processing sequence, they'll be added in the correct sequence according to their respective *)
  (* elements. And since we have to compute the score starting from the tail of the symbols list, we'll get that *)
  (* by reverting that list of symbols. *)
  let rec element_loop from_idx symbols nb_symbols : symbol list =
    if from_idx == (Array.length element_buf) then symbols
    else if nb_symbols == max_nb_symbols then symbols
    else let element = element_buf.(from_idx) in
         let skip = skip_element_fn from_idx element symbols in
         if skip then element_loop (from_idx + 1) symbols nb_symbols
         else try
             let idx, symbol = find_symbol elements element in
             element_loop (from_idx + 1) (symbol :: symbols) (nb_symbols + 1)
           with No_free_symbol_for_element err_element ->
             element_loop (from_idx + 1) symbols nb_symbols
  in
  let symbols = element_loop 0 [] 0 in
  let score = get_score symbols in      (* no need to reverse: element_loop already returns the list reversed *)
  (List.rev symbols, score)             (* but reverse list for return value, that's what the caller expects *)

let read_candidates file_path nb_elements : element array =
  let rec read_lines chan element_buf element_idx =
    try
      let line = input_line chan in
      let line = String.trim line in    (* we're parsing a file w/ DOS ending on Linux, so trim the \r *)
      element_buf.(element_idx) <- line;
      read_lines chan element_buf (element_idx + 1)
    with End_of_file ->
      close_in chan;
      element_buf
  in
  let chan = open_in file_path in
  let element_buf = Array.make nb_elements "" in
  read_lines chan element_buf 0

let write_submission file_path elements symbols =
  let rec write_lines chan = function
    | [] -> close_out chan
    | symbol :: rest_symbols ->
       let idx = make_idx symbol.[0] symbol.[1] in
       let element = elements.(idx) in
       output_string chan element;
       output_char chan '\n';
       write_lines chan rest_symbols
  in
  let chan = open_out file_path in
  write_lines chan symbols

let () = Random.self_init ()

let () =
  let nb_elements = 10_000 in
  let prob_keep = float_of_int nb_symbols /. float_of_int nb_elements in
  let candidates = read_candidates "dp275_hard_splurthian_chemistry.txt" nb_elements in
  let skip_element_fn from_idx element symbols = Random.float 1.0 > prob_keep in
  let max_score = ref 0 in
  for i = 1 to 1_000_000 do
    let elements = Array.make nb_symbols "" in
    let symbols, score = eval_score skip_element_fn elements candidates nb_symbols in
    if score > !max_score then
      begin
        write_submission "dp275_hard_splurthian_chemistry_submission.txt" elements symbols;
        max_score := score
      end
  done

(*
 * See: https://www.reddit.com/r/dailyprogrammer/comments/4t11c3/20160715_challenge_275_hard_splurthian_chemistry/
 *
 * To run a larger number of loops, it's worthwhile to use the native compiler, e.g.:
 *   ocamlopt -safe-string -warn-error +a -inline 2 -noassert -nodynlink dp275_hard_splurthian_chemistry.ml && time ./a.out && python /home/frank/src/py-src/275-solution/main.py
 * This will also check the result using the Python test script as given for the challenge.
 * The native binary will run 1.000.000 loops in ~400 sec on my T520 and I got a score of 12 with that run.
 *)

(*
 * Local Variables:
 * compile-command: "ocaml -safe-string -warn-error +a dp275_hard_splurthian_chemistry.ml"
 * End:
 *)
