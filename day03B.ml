(** Run with "$ ocaml day03B.ml" *)

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
  let compute_group_priority elf1 elf2 elf3 = 
    (* given list of strings, return list of chars common to all *)
    let get_common_letters all_strings = 
      (* find common letters between list of chars and string *)
      let rec get_common_letter_between_list_and_string l1 s1 = 
        match l1 with
        | [] -> []
        | head::tail -> 
          (match (String.contains s1 head) with
          | true -> head :: (get_common_letter_between_list_and_string tail s1)
          | false -> get_common_letter_between_list_and_string tail s1
          )
      in
      let s0 = List.nth all_strings 0 in
      let l0 = List.init (String.length s0) (String.get s0) in
      (* get intersection of (all_strings[0] as list of chars + all_strings[1]), then intersection of that with all_strings[2], ... *)
      List.fold_left get_common_letter_between_list_and_string l0 (List.tl all_strings)
    in
    let common_letters = get_common_letters [elf1;elf2;elf3] in
    let ascii_value = Char.code (List.nth common_letters 0) in
    match ascii_value < (Char.code 'a') with
    | true -> ascii_value - (Char.code 'A') + 1 + 26
    | false -> ascii_value - (Char.code 'a') + 1
  in
  let rec sum_of_priorities input = 
    match input with
    | [] -> 0
    | head::tail -> (compute_group_priority (List.nth input 0) (List.nth input 1) (List.nth input 2)) + (sum_of_priorities (List.tl (List.tl (List.tl input))))
  in
  sum_of_priorities input;;

let ans = main () in
Printf.printf "answer = %d\n" ans;;

(* 2646 *)
