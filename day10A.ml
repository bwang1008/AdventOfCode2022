(** Run with "$ ocaml day10A.ml" *)

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
  let input_lines : string list = read_lines "inputs/day10.txt" in
  let rec process_queries (input_lines : string list) (queries : int list) : int = 
    let rec process_query (input_lines : string list) (current_cycle : int) (current_register_value : int) (query : int) : int = 
      (* is query long in future, after we finished reading commands? *)
      match input_lines with
      | [] -> query * current_register_value
      | command::rest ->
        match command with
        | "noop" -> (
            match (query <= current_cycle) with
            | true -> query * current_register_value
            | false -> process_query rest (current_cycle + 1) current_register_value query
          )
        | _ -> (
            match (query <= current_cycle + 1) with
            | true -> query * current_register_value
            | false -> 
              let added_value : int = int_of_string (List.nth (String.split_on_char ' ' command) 1) in
              process_query rest (current_cycle + 2) (current_register_value + added_value) query
          )
    in
    (* end process_query *)
    match queries with
    | [] -> 0
    | query::rest_of_queries ->
      let temp : int = process_query input_lines 1 1 query in
      let () = Printf.printf "for query %d, have ans %d\n" query temp in
      (process_query input_lines 1 1 query) + (process_queries input_lines rest_of_queries)
  in
  process_queries input_lines [20;60;100;140;180;220];;

let ans = main () in
Printf.printf "answer = %d\n" ans;;

(* 15680 *)
