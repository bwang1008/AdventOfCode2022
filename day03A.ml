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
  let input = read_lines "inputs/day03.txt" in
  let compute_priority items = 
    let compartment1 = String.sub items 0 (String.length items / 2) in
    let compartment2 = String.sub items (String.length items / 2) (String.length items / 2) in
    let get_common_letter s1 s2 = 
      let rec get_common_letter_between_list_and_string l1 s1 = 
        match l1 with
        | [] -> '?'
        | head::tail -> 
          (match (String.contains s1 head) with
          | true -> head
          | false -> get_common_letter_between_list_and_string tail s1
          )
      in
      let l1 = List.init (String.length s1) (String.get s1) in
      get_common_letter_between_list_and_string l1 s2 
    in
    let common_letter = get_common_letter compartment1 compartment2 in
    let ascii_value = Char.code common_letter in
    match ascii_value < (Char.code 'a') with
    | true -> ascii_value - (Char.code 'A') + 1 + 26
    | false -> ascii_value - (Char.code 'a') + 1
  in
  let rec sum_of_priorities input = 
    match input with
    | [] -> 0
    | head::tail -> (compute_priority head) + (sum_of_priorities tail)
  in
  sum_of_priorities input;;

let ans = main () in
Printf.printf "answer = %d\n" ans;;

(* 3610 : NOPE. I mapped 'A' -> 0 instead of 27 *)
(* 7446 *)
