(** Run with "$ ocaml day13A.ml" *)

(** function to read input from file copied from https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)

(* Read in lines from a text file *)
let read_lines name : string list = 
  let ic : in_channel = open_in name in
  let try_read () : string option = 
    try
      Some (input_line ic)
    with
      End_of_file -> None
  in
  let rec loop (acc : string list) : string list = 
    match try_read () with
    | Some s -> loop (s :: acc)
    | None -> 
      let () = close_in ic in
      List.rev acc
  in
  loop [];;

let main () = 
  let input_lines : string list = read_lines "inputs/day13.txt" in
  let rec process_lines (line_index : int) (input_lines : string list) : int = 
    let right_order : int = 0 in
    let wrong_order : int = 1 in
    let undecided : int = 2 in
    let rec compare (x : string) (y : string) : int = 
      let () = Printf.printf "Compare x = %s, y = %s\n" x y in
      let is_list (a : string) : bool = ((String.get a 0) = '[') in
      let compare_integers (x : int) (y : int) : int = 
        match x < y with
        | true -> right_order
        | false ->
            match x > y with
            | true -> wrong_order
            | false -> undecided
      in
      match (is_list x, is_list y) with
      | false, false ->
        let () = Printf.printf "Hey want to cast to ints of %s %s\n" x y in
        compare_integers (int_of_string x) (int_of_string y)
      | false, true -> compare ("[" ^ x ^ "]") y
      | true, false -> compare x ("[" ^ y ^ "]")
      | true, true -> (
        let rec compare_lists (x : string list) (y : string list) : int =
          match List.length x with
          | 0 -> (
            match List.length y with
            | 0 -> undecided
            | _ -> right_order
          )
          | _ -> (
            match List.length y with
            | 0 -> wrong_order
            | _ ->
              (* at least one element in each, so get first element of each *)
              let x0 : string = List.nth x 0 in
              let y0 : string = List.nth y 0 in
              let element_comparison = compare x0 y0 in
              match element_comparison = undecided with
              | false -> element_comparison
              | true -> compare_lists (List.tl x) (List.tl y)
          )
        in
        let to_elements (a : string) : string list = 
          let () = Printf.printf "I want to_elements of %s\n" a in
          let inner : string = String.sub a 1 ((String.length a) - 2) in
          (* String.split_on_char ',' inner *)
          let rec parse_elements (inner : string) : string list = 
            let () = Printf.printf "parse_elements %s\n" inner in
            match String.length inner with
            | 0 -> []
            | _ -> (
              let first_char : char = String.get inner 0 in
              match first_char with
              | ',' -> parse_elements (String.sub inner 1 ((String.length inner) - 1))
              | '[' -> (
                let rec find_closing_bracket (word : string) (curr_index : int) (debt : int) : int = 
                  match String.get word curr_index with
                  | '[' -> find_closing_bracket word (1 + curr_index) (debt + 1)
                  | ']' -> (
                    match debt with
                    | 1 -> curr_index
                    | _ -> find_closing_bracket word (1 + curr_index) (debt - 1)
                  )
                  | _ -> find_closing_bracket word (1 + curr_index) debt
                in
                let closing_bracket_position : int = find_closing_bracket inner 0 0 in
                let sublist : string = String.sub inner 0 (closing_bracket_position + 1) in
                let rest : string = String.sub inner (closing_bracket_position + 1) ((String.length inner) - closing_bracket_position - 1) in
                sublist :: parse_elements rest
              )
              | _ -> (
                match (String.contains inner ',') with
                | true -> (
                  (* find next comma *)
                  let next_comma : int = String.index_from inner 0 ',' in
                  let sublist : string = String.sub inner 0 next_comma in
                  let rest : string = String.sub inner next_comma ((String.length inner) - next_comma) in
                  sublist :: parse_elements rest
                )
                | false -> (
                  (* take rest as an element *)
                  let sublist : string = inner in
                  let rest : string = String.empty in
                  sublist :: parse_elements rest
                )
              )
            )
          in
          (* end parse_elements *)
          let what = parse_elements inner in
          let () = List.iter (Printf.printf "%s ") what in
          parse_elements inner
        in
        (* end to_elements *)
        compare_lists (to_elements x) (to_elements y)
      )
    in
    match (line_index < List.length input_lines) with
    | false -> 0
    | true -> 
        let listA : string = List.nth input_lines line_index in
        let listB : string = List.nth input_lines (1 + line_index) in
        let comparison_result : int = compare listA listB in
        let () = Printf.printf "Line %d with result %d\n" line_index comparison_result in
        let current_ans : int = match comparison_result = right_order with
        | true -> 1 + (line_index / 3)
        | false -> 0
        in
        current_ans + process_lines (3 + line_index) input_lines
  in
  process_lines 0 input_lines;;

let ans : int = main () in
Printf.printf "answer = %d\n" ans;;

(* 5196 *)
