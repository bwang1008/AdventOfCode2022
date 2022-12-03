(** Run with "$ ocaml day01A.ml" *)

(** function to read input from file copied from https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)

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
  let input = read_lines "inputs/day01.txt" in
  let calorie_sums =
    let rec create_accumulator input answer current_sum = 
      match input with
      | [] -> current_sum :: answer
      | head::tail -> 
        (match head with
				| "" -> create_accumulator tail (current_sum :: answer) 0
				| _ -> create_accumulator tail answer (current_sum + int_of_string head)
				)
    in 
    create_accumulator input [] 0 in
  let max_calories = 
    let max_function a b =
      match a > b with
      | true -> a
      | false -> b
    in
    List.fold_left max_function 0 calorie_sums in
  max_calories;;

let ans = main () in
Printf.printf "answer = %d\n" ans;;

(* 68775 *)
