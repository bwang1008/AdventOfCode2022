(** Run with "$ ocaml day07B.ml" *)

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
  let (input_lines : string list) = read_lines "inputs/day07.txt" in
  (* process all input line by line. takes in a stack of directory sizes: directory size, with parent directories *)
  (* coming first. Return updated stack, and list of sizes of all directories *)
  (* assumes that directories are visited in DFS style, not randomly *)
  let process_input (stack : int list) (input_line : string) (final_directory_sizes : int list) : ((int list) * (int list)) = 
    let (splitted : string list) = String.split_on_char ' ' input_line in
    match (List.nth splitted 0) with
    | "$" -> (
        match (List.nth splitted 1) with
        | "cd" -> (
            match (List.nth splitted 2) with
            | ".." -> (
                let (current_directory_size : int) = List.hd stack in
                let (updated_stack : int list) = 
                  match (List.length stack > 1) with
                  | true -> 
                    let (parent_directory_size : int) = List.hd (List.tl stack) in
                    (parent_directory_size + current_directory_size) :: (List.tl (List.tl stack))
                  | false -> []
                in
                (* done with current directory. add size to final list, and pop off stack *)
                (updated_stack, current_directory_size :: final_directory_sizes)
              )
            | _ -> (0 :: stack, final_directory_sizes)     (* append on new directory, with size 0 *)
        )
        | _ -> (stack, final_directory_sizes)  (* don't care about "$ ls" *)
      )
    | "dir" -> (stack, final_directory_sizes) (* don't care about directories showing *)
    | _ ->  (* got file size first *)
      let (file_size : int) = int_of_string (List.nth splitted 0) in
      let (directory_prev_size : int) = List.hd stack in
      ((file_size + directory_prev_size) :: (List.tl stack), final_directory_sizes)
  in
  (* end process_input *)
  (* run process_input on each line of input, stimulating stack of directories' sizes. Get out list of sizes of *)
  (* all directories *)
  let rec process_all_inputs (input_lines : string list) (stack : int list) (final_directory_sizes : int list) : int list = 
    match input_lines with
    | [] -> (
        match ((List.length stack) > 0) with
        | true -> (
            (* keep "cd .." until reach root level, to update parent directories' sizes *)
            let (updated_stack_and_answer : (int list) * (int list)) = 
              process_input stack "$ cd .." final_directory_sizes
            in
            process_all_inputs input_lines (fst updated_stack_and_answer) (snd updated_stack_and_answer)
          )
        | false ->
          final_directory_sizes
      )
    | input_line::tail -> 
      (* simulate for one step *)
      let (updated_stack_and_answer : (int list) * (int list)) = 
        process_input stack input_line final_directory_sizes
      in
      (* recurse on other steps *)
      process_all_inputs tail (fst updated_stack_and_answer) (snd updated_stack_and_answer)
  in
  (* end process_all_inputs *)
  (* back in main *)
  let directory_sizes : int list = process_all_inputs input_lines [] [] in
  let total_memory : int = 70000000 in
  let required_free_memory : int = 30000000 in
  let current_used_memory : int = (List.nth directory_sizes 0) in
  let current_free_memory : int = total_memory - current_used_memory in
  let min_directory_to_be_deleted_size : int = required_free_memory - current_free_memory in
  let sorted_directory_sizes : int list = List.sort compare directory_sizes in
  let found_big_enough_directory (directory_size : int) : bool = 
    directory_size >= min_directory_to_be_deleted_size
  in
  let found_directory_size : int = List.find found_big_enough_directory sorted_directory_sizes in
  found_directory_size;;

let ans = main () in
Printf.printf "answer = %d\n" ans;;

(* 3979145 *)
