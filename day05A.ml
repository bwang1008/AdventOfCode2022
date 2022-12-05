(** Run with "$ ocaml day05A.ml" *)

(** function to read input from file copied from https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)

(* Read in lines from a text file *)
let read_lines name : string list = 
  let ic = open_in name in
  let try_read () = 
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = 
    match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop [];;

let main () = 
  let (input_lines : string list) = read_lines "inputs/day05.txt" in
  (* split input by the empty line: first half is initial stacks; second half is following commands *)
  let split_input_lines : string list * string list = 
    let rec splitter (input_lines : string list) (accumulator : string list) : (string list * string list) = 
      match input_lines with
      | [] -> (["impossible"], input_lines)
      | head::tail -> (
          match head with
          | "" -> (List.rev accumulator, tail)
          | _ -> splitter tail (head :: accumulator)
        )
    in
    splitter input_lines []
  in
  (* convert input's 2d grid representation to list of list of chars *)
  let initial_stacks : char list list = 
    (* for each line of input from bottom to top: add new letter to front of char list *)
    let rec parse_input_lines (input_lines : string list) (stacks : char list list) : char list list = 
      (* given a line of inputs of rows of crates, go through entire line of input and prepare each stack with proper char *)
      let rec parse_input_line (input_line : string) (column: int) (stacks : char list list) : char list list = 
        match (String.length input_line < 3) with
        (* does not have [A] in input *)
        | true -> stacks
        | false -> 
          let crate_char : char = String.get input_line 1 in
          (* copy stacks, but at the specified column (row), prepend a char to that list *)
          let rec copy_and_update_stacks (stacks : char list list) (column : int) (crate_char : char) (new_stacks : char list list) : char list list = 
            match stacks with
            | [] -> List.rev new_stacks
            | crates::tail -> 
              (match column with
              | 0 -> copy_and_update_stacks tail (column - 1) crate_char ((crate_char :: crates) :: new_stacks)
              | _ -> copy_and_update_stacks tail (column - 1) crate_char (crates :: new_stacks)
              )
          in
          let updated_stacks : char list list = 
            match crate_char with
            | ' ' -> stacks
            | _ -> copy_and_update_stacks stacks column crate_char [] 
          in
          (* have "[A]" as start, so go to next one *)
          match (String.length input_line >= 4) with
          | true -> parse_input_line (String.sub input_line 4 ((String.length input_line) - 4)) (column + 1) updated_stacks
          | false -> parse_input_line "" (column + 1) updated_stacks
      in
      match input_lines with
      | [] -> stacks
      | input_line::tail -> parse_input_lines tail (parse_input_line input_line 0 stacks)
    in
    (* use first part of input. reverse, get the first line ("1 2 3 ... 9"), then split by space to get ["1";"2";...;"9"], then reverse + 0th to get "9", then turn to int *)
    let num_stacks : int = 
      let splitted_numbers : string list = (List.rev (String.split_on_char ' ' (List.nth (List.rev (fst split_input_lines)) 0))) in
      let is_number (st : string) : bool =
        st <> ""
      in
      let just_numbers = List.filter is_number splitted_numbers in
      int_of_string (List.nth just_numbers 0)
    in
    let () = Printf.printf "num_stacks = %d\n" num_stacks in
    let initial_empty_stacks : char list list= 
      let empty_stack_function (i : int) : char list = 
        []
      in
      List.init num_stacks empty_stack_function
    in
    (* use first part of input. reverse, but get rid of extraneous "1 2 3 4 5 ... 9" *)
    parse_input_lines (List.tl (List.rev (fst split_input_lines))) initial_empty_stacks
  in
  (* initial_stacks end *)
  (* go through each command in the second half of the input, and simulate moving around crates in the stacks *)
  let rec simulate_commands (stacks : char list list) (commands : string list) : char list list = 
    (* given one line of command, simulate moving around crates *)
    let rec simulate_command (stacks : char list list) (command : string) : char list list = 
      let split_command : string list = String.split_on_char ' ' command in
      let num_crates : int = int_of_string (List.nth split_command 1) in
      let src_column : int = int_of_string (List.nth split_command 3) - 1 in
      let dest_column : int = int_of_string (List.nth split_command 5) - 1 in
      (* helper function to update the (column_index) column to be column_crates *)
      let set_column (stacks : char list list) (column_index : int) (column_crates : char list) : char list list = 
        let rec set_column_helper (stacks : char list list) (column_index : int) (column_crates : char list) (new_stacks : char list list) : char list list = 
          match stacks with
          | [] -> List.rev new_stacks
          | old_crates::tail ->
            match column_index with
            | 0 -> set_column_helper tail (column_index - 1) column_crates (column_crates :: new_stacks)
            | _ -> set_column_helper tail (column_index - 1) column_crates (old_crates :: new_stacks)
        in
        set_column_helper stacks column_index column_crates []
      in
      (* I don't see a sublist function of List *)
      let rec sublist (mylist : char list) (i : int) (j : int) : char list = 
        match mylist with
        | [] -> []
        | head::tail -> 
          match (i <= 0 && 0 < j) with
          | true -> head :: (sublist tail (i - 1) (j - 1))
          | false -> sublist tail (i - 1) (j - 1)
      in
      (* take num_crates away from beginning src_column: aka take everything after first num_crates letters *)
      let updated_src_column : char list = 
        sublist (List.nth stacks src_column) num_crates (List.length (List.nth stacks src_column))
      in
      (* add those crates previously removed, to the dest_column top, in reverse order *)
      let updated_dest_column : char list = 
        let removed_crates : char list = sublist (List.nth stacks src_column) 0 num_crates in
        List.append (List.rev removed_crates) (List.nth stacks dest_column)
      in
      set_column (set_column stacks src_column updated_src_column) dest_column updated_dest_column 
    in
    (* end simulate_command *)
    match commands with
    | [] -> stacks
    | command::tail -> simulate_commands (simulate_command stacks command) tail
  in
  (* end simulate_commands *)
  (* collect top letter of each stack *)
  let final_stacks : char list list = simulate_commands initial_stacks (snd split_input_lines) in
  let collected_top_letters_of_stacks : char list = 
    let rec collector (stacks : char list list) : char list = 
      match stacks with
      | [] -> []
      | crates::tail -> (List.nth crates 0) :: collector tail
    in
    collector final_stacks
  in
  (* convert to string *)
  String.init (List.length collected_top_letters_of_stacks) (List.nth collected_top_letters_of_stacks);;

let ans = main () in
Printf.printf "answer = %s\n" ans;;

(* QPJPLMNNR *)
