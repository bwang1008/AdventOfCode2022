(** Run with "$ ocaml day02B.ml" *)

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
  let input = read_lines "inputs/day02.txt" in
  let total_score = 
    let round_score hands = 
      match hands with
      | "A X" -> 3 + 0
      | "A Y" -> 1 + 3
      | "A Z" -> 2 + 6
      | "B X" -> 1 + 0
      | "B Y" -> 2 + 3
      | "B Z" -> 3 + 6
      | "C X" -> 2 + 0
      | "C Y" -> 3 + 3
      | "C Z" -> 1 + 6
      | _ -> 0
    in
    let rec compute_score input = 
      match input with
      | [] -> 0
      | head::tail -> (round_score head) + compute_score tail
    in
    compute_score input in
  total_score;;

let ans = main () in
Printf.printf "answer = %d\n" ans;;

(* 13509 *)
