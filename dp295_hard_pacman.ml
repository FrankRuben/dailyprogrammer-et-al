let time_test_1, lines_test_1 = 2, "C12"
let time_test_2, lines_test_2 = 2, "21C"
let time_test_3, lines_test_3 = 2, "31C21"

let time_sample_1, lines_sample_1 = 4, "XXXXX
X197X
X2C6X
X345X
XXXXX"

let time_sample_2, lines_sample_2 = 3, "XXXXXXXXXXXXXX
X111C2OXO2111X
XXXXXXXXXXXXXX"

let time_challenge_1, lines_challenge_1 = 10, "XXXXXXXXXXXXX
X23543561195X
X9X3X17C12X4X
X515OX1183X6X
X7865X48O585X
XXXXXXXXXXXXX"

let time_challenge_2, lines_challenge_2 = 20, "XXXXXXXXXXXXXX
XXC1212121213X
X4X21212O2121X
X44X232323232X
X444X43434343X
X4444XXXXXX77X
X4444O6789X99X
XXXXXXXXXXXXXX"

exception Cannot_find_square
exception Bad_warp_info
exception Missing_warp_info

type square_info = Start | Blocked | Warp | Point_cnt of int

let print_map (map : square_info array array) : unit =
  Array.iter (fun (squares : square_info array) ->
      Array.iter (fun (square : square_info) ->
          print_string (string_of_square_info square))
        squares;
      print_endline "")
    map;;

type point = (int * int)
let pp_point ppf (dx, dy) = Printf.fprintf ppf "(x: %d, y: %d)" dx dy

type dir = (int * int)
let pp_dir ppf (dx, dy) = Printf.fprintf ppf "(dx: %d, dy: %d)" dx dy

type warp = (point * point)

type move = Already_seen | Cannot_Reach | Done

let gather_pacgums (time : int) (start_xy : point) (warp : warp option) map : (int * point list) =
  let nb_rows = Array.length map in
  let nb_cols = Array.length map.(0) in
  let all_dir_list = [ (*up*) (0, -1); (*right*) (1, 0); (*down*) (0, 1); (*left*) (-1, 0) ] in

  let rec gather_square_dirs (time_left : int) (start_xy : point) (seen_list : point list)
      (pacgums : int) : (move * int * point list) =
    let scores = List.map
        (fun (dir_xy : dir) ->
           let move, new_pacgums, new_seen_list = gather_square time_left start_xy dir_xy seen_list pacgums in
           match move with
           | Already_seen -> (new_pacgums, new_seen_list)
           | Cannot_Reach -> (new_pacgums, new_seen_list)
           | Done -> (new_pacgums, new_seen_list))
        all_dir_list in
    let sorted = List.sort
        (fun (pacgums_1, seen_list_1) (pacgums_2, seen_list_2) -> pacgums_2 - pacgums_1 )
        scores in
    let best = List.hd sorted in
    let (best_pacgums, best_seen_list) = best in
    (Done, best_pacgums, best_seen_list)

  and gather_square (time_left : int) (start_xy : point) (dir_xy : dir) (seen_list : point list)
      (pacgums : int) : (move * int * point list) =
    if time_left == 0
    then (Done, pacgums, seen_list)
    else begin
      let (dx, dy) = dir_xy in
      let (start_x, start_y) = start_xy in
      let new_y = start_y + dy in
      if new_y >= 0 && new_y < nb_rows then
        let new_x = start_x + dx in
        if new_x >= 0 && new_x < nb_cols then begin
          let new_xy = (new_x, new_y) in
          match map.(new_y).(new_x) with
          | Start -> gather_square_dirs (time_left - 1) new_xy seen_list pacgums
          | Blocked -> (Cannot_Reach, pacgums, seen_list)
          | Warp ->
            (match warp with
             | Some (warp1_xy, warp2_xy) ->
               (* Warping is instantanious, but we still need to step into the teleporter, so decrease time. *)
               if new_xy = warp1_xy
               then gather_square_dirs (time_left - 1) warp2_xy seen_list pacgums
               else if new_xy = warp2_xy
               then gather_square_dirs (time_left - 1) warp1_xy seen_list pacgums
               else raise Bad_warp_info
             | None -> raise Missing_warp_info)
          | Point_cnt square_pacgums ->
            if List.mem new_xy seen_list (* Don't count already seen squares twice. *)
            then gather_square_dirs (time_left - 1) new_xy (new_xy :: seen_list) pacgums
            else gather_square_dirs (time_left - 1) new_xy (new_xy :: seen_list) (pacgums + square_pacgums)
        end
        else (Cannot_Reach, pacgums, seen_list)
      else (Cannot_Reach, pacgums, seen_list)
    end
  in
  let move, pacgums, seen_list = gather_square_dirs time start_xy [] 0 in
  assert (move = Done);
  (pacgums, seen_list);;

let () =
  let make_map (lines : string list) =
    let fill_map_line (map : square_info array array) (row_idx : int) (line : string) : unit =
      String.iteri (fun (col_idx : int) (square_char : char) ->
          let square_info = match square_char with
            | 'C' -> Start
            | 'X' -> Blocked
            | 'O' -> Warp
            | _ -> Point_cnt (Char.code square_char - Char.code '0')
          in
          map.(row_idx).(col_idx) <- square_info)
        line
    in
    let fill_map (map : square_info array array) (lines : string list) : square_info array array =
      List.iteri (fun (row_idx : int) (line : string) ->
          fill_map_line map row_idx line)
        lines;
      map
    in
    let nb_rows = List.length lines in
    let nb_cols = String.length (List.hd lines) in
    let map = Array.make_matrix nb_rows nb_cols Blocked in
    fill_map map lines
  in

  let find_square_xy fn (map : square_info array array) : point =
    let rec inner row_idx col_idx to_idx : int option =
      if col_idx > to_idx then None
      else if (fn (col_idx, row_idx) map.(row_idx).(col_idx))
      then Some col_idx
      else inner row_idx (col_idx + 1) to_idx
    in
    let rec outer row_idx to_idx : point =
      if row_idx > to_idx then raise Cannot_find_square
      else let nb_cols = Array.length map.(row_idx) in
        match inner row_idx 0 (nb_cols - 1) with
        | None -> outer (row_idx + 1) to_idx
        | Some col_idx -> (col_idx, row_idx)
    in
    let nb_rows = Array.length map in
    outer 0 (nb_rows - 1)
  in

  let find_start_xy (map : square_info array array) : point =
    find_square_xy (fun xy square -> match square with | Start -> true | _ -> false) map
  in
  let find_first_warp_xy (map : square_info array array) : point option =
    try
      Some (find_square_xy (fun xy square -> match square with | Warp -> true | _ -> false) map)
    with Cannot_find_square -> None
  in
  let find_second_warp_xy (map : square_info array array) (other_xy : point) : point =
    find_square_xy
      (fun (xy : point) square ->
         if xy = other_xy
         then false
         else match square with | Warp -> true | _ -> false)
      map
  in

  let handle_map time lines (exp_pacgums : int) =
    let map = (make_map (Str.split (Str.regexp "[\n]") lines)) in
    let start_xy = find_start_xy map in
    let warp1_xy = find_first_warp_xy map in
    let warp2_xy = match warp1_xy with None -> None | Some other -> Some (find_second_warp_xy map other) in
    let warp = match warp1_xy, warp2_xy with Some warp1_xy, Some warp2_xy -> Some (warp1_xy, warp2_xy) | _ -> None in
    let pacgums, seen_list = gather_pacgums time start_xy warp map in
    assert (pacgums == pacgums);
  in
  handle_map time_test_1 lines_test_1 3;
  handle_map time_test_2 lines_test_2 3;
  handle_map time_test_3 lines_test_3 4;
  handle_map time_sample_1 lines_sample_1 4;
  handle_map time_sample_2 lines_sample_2 27;
  handle_map time_challenge_1 lines_challenge_1 54;
  handle_map time_challenge_2 lines_challenge_2 76;;

(*
 * See: https://www.reddit.com/r/dailyprogrammer/comments/5iq4ix/20161216_challenge_295_hard_advanced_pacman/
 *)
 
(*
 * Local Variables:
 * compile-command: "ocaml -safe-string -strict-sequence -strict-formats -warn-error +a str.cma dp295_hard_pacman.ml"
 * End:
 *)
