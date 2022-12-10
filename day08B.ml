(** Run with "$ ocaml day08B.ml" *)

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
  (* let print_int_list (row : int list) =  *)
    (* let () = List.iter (Printf.printf "%d ") row in *)
    (* let () = Printf.printf "\n" in *)
    (* () *)
  (* in *)
  (* let print_int_matrix (matrix : int list list) =  *)
    (* let () = List.iter print_int_list matrix in *)
    (* let () = Printf.printf "\n" in *)
    (* () *)
  (* in *)
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
  let set_list_index (my_list : int list) (index : int) (element : int) : int list = 
    let helper_function (i : int) (original_element : int) : int = 
      match i with
      | x when x = index -> element
      | _ -> original_element
    in
    List.mapi helper_function my_list
  in
  (* given a sublist [left, right) from my_list, get the index of the highest value. If multiple, get the right-most one *)
  let get_index_of_highest_value (my_list : int list) (left : int) (right : int) : int = 
    (* turn values outside of range to 0 *)
    let turn_outside_of_range_to_neg : int list = 
      let helper (index : int) (element : int) : int = 
        match (left <= index && index < right) with
        | true -> element
        | false -> (-1)
      in
      List.mapi helper my_list
    in
    let last_argmax (my_list : int list) : int = 
      let rec helper (my_list : int list) (index : int) (curr_answer : int) : int = 
        match index with
        | x when x = List.length my_list -> curr_answer
        | _ -> 
          match ((List.nth my_list index) >= (List.nth my_list curr_answer)) with
          | true -> helper my_list (1 + index) index
          | false -> helper my_list (1 + index) curr_answer
      in
      helper my_list 0 0
    in
    (* get right-most index that has value == max_value *)
    last_argmax turn_outside_of_range_to_neg
  in
  (* given a row of tree heights, return list of same length, where ans[i] == number of trees to the right tree[i] can see *)
  let process_row_to_get_rightmost_tree (row : int list) : int list = 
    let rec process_row_helper (row : int list) (left : int) (right : int) (answers : int list) (prev_highest_index : int) : int list =
      match (left < right) with
      | false -> answers
      | true ->
        (* get index of highest value in sublist [left, right) of row. In case of ties, pick right most / highest index *)
        let index_of_highest : int = get_index_of_highest_value row left right in

        (* this tree is the highest in this range. The highest tree to its right is at prev_highest_index *)
        let answers : int list = set_list_index answers index_of_highest prev_highest_index in
        (* recurse on left range *)
        let answers : int list = process_row_helper row left (index_of_highest) answers index_of_highest in
        (* recurse on right range *)
        let answers : int list = process_row_helper row (index_of_highest + 1) right answers prev_highest_index in
        answers
    in
    let row_length : int = List.length row in
    let next_neighbor_list : int list = process_row_helper row 0 row_length (List.init row_length (fun (x : int) : int -> 0)) (row_length - 1) in
    (* now have list, where list[i] is position of next higher neighbor. Ex: 30373 would be [2,1,3,4,4], because for first 3, its next higher or equal number is the next 3 at position 2 *)
    let helper (index : int) (element : int) : int =
      let next_neighbor : int = element in
      let diff : int = next_neighbor - index in
      diff
    in
    let temp = List.mapi helper next_neighbor_list in
    (* let () = Printf.printf "Original = \n" in *)
    (* let () = print_int_list row in *)
    (* let () = Printf.printf "converted to \n" in *)
    (* let () = print_int_list next_neighbor_list in *)
    (* let () = Printf.printf "converted to \n" in *)
    (* let () = print_int_list temp in *)
    temp
  in
  let rec tree_to_right_helper (tree_grid : int list list) : int list list =
    match tree_grid with
    | [] -> []
    | row::tail -> (process_row_to_get_rightmost_tree row) :: (tree_to_right_helper tail)
  in
  let rec tree_to_left_helper (tree_grid : int list list) : int list list =
    match tree_grid with
    | [] -> []
    | row::tail -> (List.rev (process_row_to_get_rightmost_tree (List.rev row))) :: (tree_to_left_helper tail)
  in
  let tree_to_right : int list list = tree_to_right_helper tree_grid in
  let tree_to_left : int list list = tree_to_left_helper tree_grid in
  let transposed_tree_grid : int list list = transpose tree_grid in
  let tree_to_bottom : int list list = transpose (tree_to_right_helper transposed_tree_grid) in
  let tree_to_top : int list list = transpose (tree_to_left_helper transposed_tree_grid) in
  (* let () = Printf.printf "To right:\n" in *)
  (* let () = print_int_matrix tree_to_right in *)
  (* let () = Printf.printf "\n" in *)
  (* let () = Printf.printf "To left:\n" in *)
  (* let () = print_int_matrix tree_to_left in *)
  (* let () = Printf.printf "\n" in *)
  (* let () = Printf.printf "To down:\n" in *)
  (* let () = print_int_matrix tree_to_bottom in *)
  (* let () = Printf.printf "\n" in *)
  (* let () = Printf.printf "To top:\n" in *)
  (* let () = print_int_matrix tree_to_top in *)
  (* let () = Printf.printf "\n" in *)
  let max_tree_score : int = 
    let tree_to_right : int list = List.concat tree_to_right in
    let tree_to_left : int list = List.concat tree_to_left in
    let tree_to_bottom : int list = List.concat tree_to_bottom in
    let tree_to_top : int list = List.concat tree_to_top in
    let rec traverse_trees (index : int) (curr_score : int) : int =
      match index with
      | x when x = List.length tree_to_left -> curr_score
      | _ -> 
        let score : int = (List.nth tree_to_left index) * (List.nth tree_to_right index) * (List.nth tree_to_top index) * (List.nth tree_to_bottom index) in
        traverse_trees (1 + index) (max curr_score score)
    in
    traverse_trees 0 0
  in
  max_tree_score;;

let ans = main () in
Printf.printf "answer = %d\n" ans;;

(* 180000 *)
