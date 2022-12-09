(** Run with "$ ocaml day08A.ml" *)

(** function to read input from file copied from https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)

(* Read in lines from a text file *)
let read_lines name : string list = 
  let ic : in_channel = open_in name in
  let try_read () : string option = 
    try
      Some (input_line ic)
    with
      End_of_file -> None
  in
  let rec loop (acc : string list) : string list = 
    match try_read () with
    | Some s -> loop (s :: acc)
    | None -> 
      let () = close_in ic in
      List.rev acc
  in
  loop [];;

let main () = 
  let input_lines : string list = read_lines "inputs/day08.txt" in
  let tree_grid : int list list =
    let parse_line (line : string) : int list =
      let char_to_int (c : char) : int = 
        (int_of_char c) - (int_of_char '0')
      in
      List.map char_to_int (List.init (String.length line) (String.get line))
    in
    let rec process_inputs (input_lines : string list) : int list list =
      match input_lines with
      | [] -> []
      | line::tail -> (parse_line line) :: process_inputs tail
    in
    process_inputs input_lines
  in
  (* let print_bool_list (row : bool list) =  *)
    (* let () = List.iter (Printf.printf "%d ") (List.map (function | true -> 1 | false -> 0) row) in *)
    (* let () = Printf.printf "\n" in *)
    (* () *)
  (* in *)
  (* let print_int_list (row : int list) =  *)
    (* let () = List.iter (Printf.printf "%d ") row in *)
    (* let () = Printf.printf "\n" in *)
    (* () *)
  (* in *)
  let process_row (row : int list) : bool list = 
    (* let () = Printf.printf "Doing row: " in *)
    (* let () = List.iter (Printf.printf "%d ") row in *)
    (* let () = Printf.printf "\n" in *)
    let rec process_row_helper (row : int list) (index : int) (curr_highest : int) : bool list =
      match index with
      | x when x = List.length row -> []
      | _ -> (curr_highest < (List.nth row index)) :: process_row_helper row (1 + index) (max curr_highest (List.nth row index))
    in
    let temp = process_row_helper row 0 (-1) in
    (* let () = Printf.printf "returns " in *)
    (* let () = print_bool_list temp in *)
    temp
  in
  let transpose (matrix : int list list) : int list list =
    let get_column (matrix : int list list) (col_index : int) : int list =
      List.map (fun (row : int list) : int -> List.nth row col_index) matrix
    in
    let num_columns : int = List.length (List.nth matrix 0) in
    let rec generate_all_columns (matrix : int list list) (column_index : int) : int list list =
      match column_index with
      | x when x = num_columns -> []
      | _ -> (get_column matrix column_index) :: generate_all_columns matrix (1 + column_index)
    in
    generate_all_columns matrix 0
  in
  let transpose_bool_matrix (matrix : bool list list) : bool list list =
    let get_column (matrix : bool list list) (col_index : int) : bool list =
      List.map (fun (row : bool list) : bool -> List.nth row col_index) matrix
    in
    let num_columns : int = List.length (List.nth matrix 0) in
    let rec generate_all_columns (matrix : bool list list) (column_index : int) : bool list list =
      match column_index with
      | x when x = num_columns -> []
      | _ -> (get_column matrix column_index) :: generate_all_columns matrix (1 + column_index)
    in
    generate_all_columns matrix 0
  in
  (* let print_bool_matrix (matrix : bool list list) =  *)
    (* List.iter (fun (row : bool list) -> List.iter (Printf.printf "%d ") (List.map (function | true -> 1 | false -> 0) row); Printf.printf "\n") matrix *)
  (* in *)
  let rec visible_from_left_helper (tree_grid : int list list) : bool list list = 
    match tree_grid with
    | [] -> []
    | row::tail -> 
      (process_row row) :: (visible_from_left_helper tail)
  in
  let rec visible_from_right_helper (tree_grid : int list list) : bool list list = 
    match tree_grid with
    | [] -> []
    | row::tail -> (List.rev (process_row (List.rev row))) :: visible_from_right_helper tail
  in
  let visible_from_left : bool list list = visible_from_left_helper tree_grid in
  let visible_from_right : bool list list = visible_from_right_helper tree_grid in
  let transposed_tree_grid : int list list = transpose tree_grid in
  let visible_from_top : bool list list = transpose_bool_matrix (visible_from_left_helper transposed_tree_grid) in
  let visible_from_bottom : bool list list = transpose_bool_matrix (visible_from_right_helper transposed_tree_grid) in
  (* let () = print_bool_matrix visible_from_left in *)
  (* let () = Printf.printf "\n" in *)
  (* let () = print_bool_matrix visible_from_right in *)
  (* let () = Printf.printf "\n" in *)
  (* let () = print_bool_matrix visible_from_top in *)
  (* let () = Printf.printf "\n" in *)
  (* let () = print_bool_matrix visible_from_bottom in *)
  (* let () = Printf.printf "\n" in *)
  let num_visible_trees : int = 
    let visible_from_left : bool list = List.concat visible_from_left in
    let visible_from_right : bool list = List.concat visible_from_right in
    let visible_from_top : bool list = List.concat visible_from_top in
    let visible_from_bottom : bool list = List.concat visible_from_bottom in
    let rec traverse_trees (index : int) : int =
      match index with
      | x when x = List.length visible_from_left -> 0
      | _ -> 
        let is_visible : int = 
          match (List.nth visible_from_left index) || (List.nth visible_from_right index) || (List.nth visible_from_top index) || (List.nth visible_from_bottom index) with
          | true -> 1
          | false -> 0
        in
        is_visible + traverse_trees (1 + index)
    in
    traverse_trees 0
  in
  num_visible_trees;;

let ans = main () in
Printf.printf "answer = %d\n" ans;;

(* 1943 : NOPE. Too high. EDIT: Had some lists backwards, like my inputs was backwards, and forgot to transpose results of visible_from_top/bottom *)
(* 1843 *)
