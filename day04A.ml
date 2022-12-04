(** Run with "$ ocaml day04A.ml" *)

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
  let (input : string list) = read_lines "inputs/day04.txt" in
  (* turn "2-8" to (2, 8) *)
  let parse_range (input : string) : (int * int) = 
    let (mylist : int list) = List.map int_of_string (String.split_on_char '-' input) in
    ((List.nth mylist 0), (List.nth mylist 1))
  in
  (* convert line of input, to two ranges, and see if one is contained in another *)
  let parse_line (input : string) : bool = 
    let (splitted : string list) = String.split_on_char ',' input in
    let (range1 : (int * int)) = parse_range (List.nth splitted 0) in
    let (range2 : (int * int)) = parse_range (List.nth splitted 1) in
    ((fst range1) <= (fst range2) && (snd range2) <= (snd range1)) || ((fst range2) <= (fst range1) && (snd range1) <= (snd range2))
  in
  (* for each line of input, if range in another, add 1 *)
  let rec parse_inputs (input : string list) : int = 
    match input with
    | [] -> 0
    | head::tail -> (if (parse_line head) then 1 else 0) + parse_inputs tail
  in
  parse_inputs input;;

let ans = main () in
Printf.printf "answer = %d\n" ans;;

(* 576 *)
