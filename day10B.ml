(** Run with "$ ocaml day10B.ml" *)

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
  let height = 6 in
  let width = 40 in
  let abs (x : int) : int = 
    match (x >= 0) with
    | true -> x
    | false -> (-x)
  in
  let rec simulate (commands : string list) (curr_cycle : int) (curr_register_value : int) (iteration_on_same_command : int) (screen : char list) : (string list) * int * int * int * (char list) =
    (* let () = Printf.printf "\nStart: command %s curr_cycle %d with X = %d on iter %d\n" (List.hd commands) curr_cycle curr_register_value iteration_on_same_command in *)
    match curr_cycle with
    | x when x = height * width -> (commands, curr_cycle, curr_register_value, iteration_on_same_command, screen)
    | _ -> (
      match commands with
      | [] -> simulate commands (1 + curr_cycle) curr_register_value iteration_on_same_command screen
      | command::rest_of_commands -> 
        let pixel : char = 
          let overlap : bool = 
            let cursor : int = (curr_cycle mod width) in
            let sprite_center : int = curr_register_value in
            (abs (sprite_center - cursor)) <= 1
          in
          match overlap with
          | true -> '#'
          | false -> '.'
        in
        let updated_screen = List.rev (pixel :: (List.rev screen)) in
        match command with
        | "noop" -> simulate rest_of_commands (1 + curr_cycle) curr_register_value 1 updated_screen
        | _ -> 
          let updated_value : int = int_of_string (List.nth (String.split_on_char ' ' command) 1) in
          match iteration_on_same_command with
          | 2 -> simulate rest_of_commands (1 + curr_cycle) (curr_register_value + updated_value) 1 updated_screen
          | _ -> simulate commands (1 + curr_cycle) curr_register_value (1 + iteration_on_same_command) updated_screen
    )
  in
  (* end simulate *)
  let (_, _, _, _, updated_screen) = simulate input_lines 0 1 1 [] in
  let print_list (screen : char list) (width : int) : unit =
    let helper (index : int) (pixel : char) : unit =
      let () = Printf.printf "%c" pixel in
      let () =
        match ((index + 1) mod width) with
        | 0 -> Printf.printf "\n"
        | _ -> ()
      in
      ()
    in
    List.iteri helper screen
  in
  let () = Printf.printf "Updated_screen size = %d\n" (List.length updated_screen) in
  print_list updated_screen width;;

main ();;

(* ####.####.###..####.#..#..##..#..#.###.. *)
(* ...#.#....#..#.#....#..#.#..#.#..#.#..#. *)
(* ..#..###..###..###..####.#....#..#.#..#. *)
(* .#...#....#..#.#....#..#.#.##.#..#.###.. *)
(* #....#....#..#.#....#..#.#..#.#..#.#.... *)
(* ####.#....###..#....#..#..###..##..#.... *)

(* ZFBFHGUP *)
