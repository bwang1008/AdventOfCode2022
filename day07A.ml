(** Run with "$ ocaml day07A.ml" *)

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

(* module CharSet = Set.Make(Char);; *)

let main () = 
  let (input_lines : string list) = read_lines "inputs/day07.txt" in
  (* process all input line by line. takes in a stack of directories: (directory name * directory size). return updated stack, *)
  (* as well as answer of sum of directories that have less than 100000 bytes *)
  (* assumes that directories are visited in DFS style, not randomly *)
  let process_input (stack : (string * int) list) (input_line : string) (current_answer : int) : (((string * int) list) * int) = 
    (* let () = Printf.printf "Process input %s\n" input_line in *)
    let (splitted : string list) = String.split_on_char ' ' input_line in
    match (List.nth splitted 0) with
    | "$" -> (
        match (List.nth splitted 1) with
        | "cd" -> (
            match (List.nth splitted 2) with
            | ".." -> (
                let (current_directory_size : int) = snd (List.hd stack) in
                let (updated_stack : (string * int) list) = 
                  let (parent_directory_size : int) = snd (List.hd (List.tl stack)) in
                  (fst (List.hd (List.tl stack)), parent_directory_size + current_directory_size) :: (List.tl (List.tl stack))
                in
                (* check current directory: if size <= 100000, add to answer *)
                match (current_directory_size <= 100000) with
                | true -> (updated_stack, current_answer + current_directory_size)
                | false -> (updated_stack, current_answer)
            )
            | _ -> (((List.nth splitted 2), 0) :: stack, current_answer)     (* append on new directory, with size 0 *)
        )
        | _ -> (stack, current_answer)  (* don't care about "$ ls" *)
      )
    | "dir" -> (stack, current_answer) (* don't care about directories showing *)
    | _ ->  (* got file size first *)
      let (file_size : int) = int_of_string (List.nth splitted 0) in
      let (directory_prev_size : int) = (snd (List.hd stack)) in
      (((fst (List.hd stack)), file_size + directory_prev_size) :: (List.tl stack), current_answer)
  in
  (* end process_input *)
  (* run process_input on each line of input, stimulating stack of directories and their sizes. Get out final answer of *)
  (* sum of all directory sizes <= 100,000 *)
  let rec process_all_inputs (input_lines : string list) (stack : (string * int) list) (current_answer : int) : int = 
    match input_lines with
    | [] -> (
        match ((List.length stack) > 1) with
        | true -> (
            (* keep "cd .." until reach root level, to update parent directories' sizes *)
            let (updated_stack_and_answer : (((string * int) list) * int)) = 
              process_input stack "$ cd .." current_answer
            in
            process_all_inputs input_lines (fst updated_stack_and_answer) (snd updated_stack_and_answer)
          )
        | false ->
          current_answer
      )
    | input_line::tail -> 
      (* simulate for one step *)
      let (updated_stack_and_answer : (((string * int) list) * int)) = 
        process_input stack input_line current_answer
      in
      (* let print_string_and_int (x : string * int) =  *)
        (* Printf.printf "(%s, %d) " (fst x) (snd x) *)
      (* in *)
      (* let () = List.iter print_string_and_int (fst updated_stack_and_answer) in *)
      (* let () = Printf.printf "\n" in *)
      (* recurse on other steps *)
      process_all_inputs tail (fst updated_stack_and_answer) (snd updated_stack_and_answer)
  in
  (* end process_all_inputs *)
  process_all_inputs input_lines [] 0;;

let ans = main () in
Printf.printf "answer = %d\n" ans;;

(* 1755820 : NOPE. Too low : I forgot to keep "$ cd .." until first directory to update their sizes *)
(* 1908462 *)
