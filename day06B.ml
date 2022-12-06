(** Run with "$ ocaml day06B.ml" *)

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

module CharSet = Set.Make(Char);;

let main () = 
  let (input_lines : string list) = read_lines "inputs/day06.txt" in
  let (input : string) = List.nth input_lines 0 in
  let find_first_n_mismatches (input : string) (n: int) : int = 
    let check_if_all_unique_letters (input : string) : bool =
      (* let () = Printf.printf "check if distinct %s\n" input in *)
      let add_char_to_set (myset : CharSet.t) (c : char) : CharSet.t =
        CharSet.add c myset
      in
      let (generated_set : CharSet.t) = String.fold_left add_char_to_set CharSet.empty input in
      CharSet.cardinal generated_set = String.length input
    in
    let (all_chars : char list) = List.init (String.length input) (String.get input) in
    (* convenience function, that determines if n chars that end at index of input, are all distinct. if so, return index *)
    let decider (index : int) (c : char) (input : string) (n : int) : int = 
      match (index - n + 1 >= 0) with
      | false -> -1
      | true -> 
        match (check_if_all_unique_letters (String.sub input (index - n + 1) n)) with
        | true -> index
        | false -> -1
    in
    (* List.mapi's function should have two arguments *)
    let decider_wrapper (index : int) (c : char) : int = 
      decider index c input n
    in
    let (mapped : int list) = List.mapi decider_wrapper all_chars in
    (* let () = List.iter (Printf.printf "%d ") mapped in *)
    let is_nonnegative (n : int) : bool = 
      n >= 0
    in
    (* 0 to 1-indexed *)
    1 + List.find is_nonnegative mapped
  in
  find_first_n_mismatches input 14;;

let ans = main () in
Printf.printf "answer = %d\n" ans;;

(* 2250 *)
