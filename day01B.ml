(** Run with "$ ocaml day01B.ml" *)

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
  let reverse_comparison a b = (- compare a b) in
  let sorted_calorie_sums = List.sort reverse_comparison calorie_sums in
  (List.nth sorted_calorie_sums 0) + (List.nth sorted_calorie_sums 1) + (List.nth sorted_calorie_sums 2);;
  
let ans = main ();;
Printf.printf "answer is %d\n" ans;;

(* answer = 202585 *)
