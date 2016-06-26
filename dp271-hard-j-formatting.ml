let j1 = "
  D=. x +/&i.&>:&# y
  for_i. 1+i.#x do.
    for_j. 1+i.#y do.
      if. ((<:i){x)=(<:j){y do.
        D=.(D {~<<:i,j) (<i,j)} D
      else.
        min=. 1+<./D{~(i,j) <@:-\"1#:1 2 3
        D=. min (<i,j)} D
      end.
    end.
  end.
  {:{:D";;

let j2 = "D=. x +/&i.&>:&# y for_i. 1+i.#x do. for_j. 1+i.#y do. if. ((<:i) (x)=(<:j){y do. D=.(D {~<<:i,j) (<i,j)} D else. min=. 1+<./D{~(i,j) <@:-\"1#:1 2 3 label_. D=. min (<i,j)} D end. end. end. {:{:D";;

type out_format_type = Basic | Linear
type in_stmt_type = For | If | Else | End
type out_stmt_type = { stmt_words: string list; level: int; needs_label: bool }
type label_info = No_label | In_stmt_cnt of int

exception Pop_empty_stmt_stack of int * in_stmt_type
exception Unexpected_Top_Of_Stmt_Stack of int * in_stmt_type * in_stmt_type

let starts_with (s1 : string) (s2 : string) : bool =
  let l1 = String.length s1
  and l2 = String.length s2 in
  if l1 < l2 then false else ((String.sub s1 0 l2) = s2);;

let process_lines (lines : string list) : out_stmt_type list =

  let push_in_stmt line_nb new_stmt in_stmt_stack =
    (new_stmt :: in_stmt_stack, List.length in_stmt_stack);
  in

  let pop_in_stmt line_nb new_stmt exp_stmt_list = function
    | [] -> raise (Pop_empty_stmt_stack (line_nb, new_stmt))
    | hd :: rest_stmts ->
      if not (List.mem hd exp_stmt_list) then raise (Unexpected_Top_Of_Stmt_Stack (line_nb, new_stmt, hd));
      (rest_stmts, List.length rest_stmts);
  in

  let complete_out_stmt in_stmt_words in_stmt_stack out_stmt_list ?(label_info=No_label) last_stmt_level =
    ({
      stmt_words = List.rev in_stmt_words;
      level = last_stmt_level;
      needs_label = match label_info with
        | No_label -> false
        | In_stmt_cnt cnt -> cnt > 1;
    } :: out_stmt_list,
      List.length in_stmt_stack);
  in

  let maybe_complete_out_stmt in_stmt_words in_stmt_stack out_stmt_list ?(label_info=No_label) last_stmt_level =
    if (List.length in_stmt_words) > 0 then
      let out_stmt_list, last_stmt_level
        = complete_out_stmt in_stmt_words in_stmt_stack out_stmt_list ~label_info last_stmt_level in
      (out_stmt_list, last_stmt_level)
    else
      (out_stmt_list, last_stmt_level)
  in

  let rec process_words line_nb in_stmt_words in_stmt_stack out_stmt_list in_stmt_cnt last_stmt_level = function
    | "if." :: rest_words ->
      let out_stmt_list, last_stmt_level
        = maybe_complete_out_stmt in_stmt_words in_stmt_stack out_stmt_list last_stmt_level in
      let in_stmt_stack, last_stmt_level = push_in_stmt line_nb If in_stmt_stack in
      process_words line_nb ["if."] in_stmt_stack out_stmt_list 0 last_stmt_level rest_words;
    | "else." :: rest_words ->
      let out_stmt_list, last_stmt_level
        = maybe_complete_out_stmt in_stmt_words in_stmt_stack out_stmt_list last_stmt_level in
      let in_stmt_stack, last_stmt_level = pop_in_stmt line_nb Else [If] in_stmt_stack in
      let in_stmt_stack, last_stmt_level = push_in_stmt line_nb  Else in_stmt_stack in
      let out_stmt_list, last_stmt_level
        = complete_out_stmt ["else."] in_stmt_stack out_stmt_list last_stmt_level in
      process_words line_nb [] in_stmt_stack out_stmt_list 0 last_stmt_level rest_words;
    | hd :: rest_words when starts_with hd "for_" ->
      let out_stmt_list, last_stmt_level
        = maybe_complete_out_stmt in_stmt_words in_stmt_stack out_stmt_list last_stmt_level in
      let in_stmt_stack, last_stmt_level = push_in_stmt line_nb For in_stmt_stack in
      process_words line_nb [hd] in_stmt_stack out_stmt_list 0 last_stmt_level rest_words;
    | "end." :: rest_words ->
      let out_stmt_list, last_stmt_level
        = maybe_complete_out_stmt in_stmt_words in_stmt_stack out_stmt_list ~label_info:(In_stmt_cnt (in_stmt_cnt)) last_stmt_level in
      let in_stmt_stack, last_stmt_level = pop_in_stmt line_nb End [If; Else; For] in_stmt_stack in
      let out_stmt_list, last_stmt_level
        = complete_out_stmt ["end."] in_stmt_stack out_stmt_list last_stmt_level in
      process_words line_nb [] in_stmt_stack out_stmt_list 0 last_stmt_level rest_words;
    | "do." :: rest_words ->
      let out_stmt_list, last_stmt_level
        = complete_out_stmt ("do." :: in_stmt_words) in_stmt_stack out_stmt_list last_stmt_level in
      process_words line_nb [] in_stmt_stack out_stmt_list 0 last_stmt_level rest_words;
    | "label_." :: rest_words ->
      let out_stmt_list, last_stmt_level
        = maybe_complete_out_stmt in_stmt_words in_stmt_stack out_stmt_list last_stmt_level in
      process_words line_nb [] in_stmt_stack out_stmt_list 2 last_stmt_level rest_words;
    | hd :: rest_words ->
      process_words line_nb (hd :: in_stmt_words) in_stmt_stack out_stmt_list in_stmt_cnt last_stmt_level rest_words;
    | [] ->
      let out_stmt_list, last_stmt_level
        = maybe_complete_out_stmt in_stmt_words in_stmt_stack out_stmt_list ~label_info:(In_stmt_cnt (in_stmt_cnt)) last_stmt_level in
      (in_stmt_stack, out_stmt_list, in_stmt_cnt + 1, last_stmt_level);
  in

  let rec process_line line line_nb in_stmt_stack out_stmt_list in_stmt_cnt last_stmt_level =
    let words = (Str.split (Str.regexp "[ \t]") line) in
    let non_empty_words = List.filter (fun s -> (String.length s) > 0) words in
    process_words line_nb [] in_stmt_stack out_stmt_list in_stmt_cnt last_stmt_level non_empty_words in

  let rec aux_process_lines lines line_nb in_stmt_stack out_stmt_list in_stmt_cnt last_stmt_level =
    match lines with
    | [] -> out_stmt_list;
    | line :: rest_lines ->
      let in_stmt_stack, out_stmt_list, in_stmt_cnt, last_stmt_level
        = process_line line line_nb in_stmt_stack out_stmt_list in_stmt_cnt last_stmt_level in
      aux_process_lines rest_lines (line_nb + 1) in_stmt_stack out_stmt_list in_stmt_cnt last_stmt_level
  in

  aux_process_lines lines 1 [] [] 0 0;;

let () =
  let out_format = Linear in
  let lines = (Str.split (Str.regexp "[\n]") j2) in

  let print_out_stmt out_stmt =
    Printf.printf "STMT: %d, %B, %d: " (List.length out_stmt.stmt_words) out_stmt.needs_label out_stmt.level;
    (match out_format with
     | Basic -> Printf.printf "%*s" (out_stmt.level * 2) ""
     | Linear when out_stmt.needs_label -> Printf.printf "label_. "
     | Linear -> ());
    List.iteri
      (fun i word -> if i > 0 then Printf.printf " %s" word else Printf.printf "%s" word)
      out_stmt.stmt_words;
    (match out_format with
     | Basic -> Printf.printf "\n"
     | Linear -> Printf.printf " ");
  in

  let out_stmt_list = process_lines lines in
  List.iter print_out_stmt (List.rev out_stmt_list);;

(*
 * See: https://www.reddit.com/r/dailyprogrammer/comments/4ojbgq/20160617_challenge_271_hard_formatting_j_code/
 * Output:
 
Linear, j1:
    D=. x +/&i.&>:&# y for_i. 1+i.#x do. for_j. 1+i.#y do. if. ((<:i){x)=(<:j){y do. D=.(D {~<<:i,j) (<i,j)} D else. min=. 1+<./D{~(i,j) <@:-"1#:1 2 3 label_. D=. min (<i,j)} D end. end. end. {:{:D 
    
Basic, j1:
    D=. x +/&i.&>:&# y
    for_i. 1+i.#x do.
      for_j. 1+i.#y do.
        if. ((<:i){x)=(<:j){y do.
          D=.(D {~<<:i,j) (<i,j)} D
        else.
          min=. 1+<./D{~(i,j) <@:-"1#:1 2 3
          D=. min (<i,j)} D
        end.
      end.
    end.
    {:{:D
    
Linear, j2:
    D=. x +/&i.&>:&# y for_i. 1+i.#x do. for_j. 1+i.#y do. if. ((<:i) (x)=(<:j){y do. D=.(D {~<<:i,j) (<i,j)} D else. min=. 1+<./D{~(i,j) <@:-"1#:1 2 3 label_. D=. min (<i,j)} D end. end. end. {:{:D 
    
Basic, j2:
    D=. x +/&i.&>:&# y
    for_i. 1+i.#x do.
      for_j. 1+i.#y do.
        if. ((<:i) (x)=(<:j){y do.
          D=.(D {~<<:i,j) (<i,j)} D
        else.
          min=. 1+<./D{~(i,j) <@:-"1#:1 2 3
          D=. min (<i,j)} D
        end.
      end.
    end.
    {:{:D
*)
 
(*
 * Local Variables:
 * compile-command: "ocaml -safe-string -strict-sequence str.cma dp271-hard-j-formatting.ml"
 * End:
 *)
