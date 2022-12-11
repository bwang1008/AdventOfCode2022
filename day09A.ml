(** Run with "$ ocaml day09A.ml" *)

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
  let input_lines : string list = read_lines "inputs/day09.txt" in
  (* given head and tail positions, move head by (dx,dy), and move tail *)
  (* return (head, tail) *)
  let move_head (dx : int) (dy : int) (head : int * int) (tail : int * int) : (int * int) * (int * int) = 
    let is_touching (head : int * int) (tail : int * int) : bool = 
      let abs (x : int) : int = 
        match (x >= 0) with
        | true -> x
        | false -> (-x)
      in
      (max (abs ((fst head) - (fst tail))) (abs ((snd head) - (snd tail)))) <= 1
    in
    let updated_head : int * int = ((fst head) + dx, (snd head) + dy) in
    let cap_value_at_max_1 (x : int) : int = 
      match x with
      | 0 -> 0
      | _ -> 
        match (x > 0) with
        | true -> 1
        | false -> -1
    in
    let tail_dx : int = cap_value_at_max_1 ((fst updated_head) - (fst tail)) in
    let tail_dy : int = cap_value_at_max_1 ((snd updated_head) - (snd tail)) in
    let updated_tail : int * int =
      match is_touching (updated_head) (tail) with
      | true -> tail
      | false -> ((fst tail) + tail_dx, (snd tail) + tail_dy)
    in
    (* let () = Printf.printf "Head moved from (%d, %d) to (%d, %d)\n" (fst head) (snd head) (fst updated_head) (snd updated_head) in *)
    (* let () = Printf.printf "tail moved from (%d, %d) to (%d, %d)\n" (fst tail) (snd tail) (fst updated_tail) (snd updated_tail) in *)
    (updated_head, updated_tail)
  in
  (* go through each line of input; return list of positions visited by tail (possibly duplicates) *)
  let rec process_inputs (input_lines : string list) (head : int * int) (tail : int * int) : (int * int) list = 
    match input_lines with
    | [] -> []
    | line::rest -> 
      (* turn command "L 5" to (-1 0 5), where 1 means to move left in dx, 0 to move 0 in dy, 5 to move (dx,dy) 5 times *)
      let command : (int * int) * int = 
        let splitted : string list = String.split_on_char ' ' line in
        let direction : string = List.nth splitted 0 in
        let dxdy : (int * int) =
          match direction with
          | "R" -> (1, 0)
          | "L" -> ((-1), 0)
          | "U" -> (0, 1)
          | "D" -> (0, (-1))
          | _ -> (0, 0)
        in
        let num_times : int = int_of_string (List.nth splitted 1) in
        (dxdy, num_times)
      in
      let rec move_n_times (num_times : int) (dx : int) (dy : int) (head : int * int) (tail : int * int) (visited : (int * int) list) : (int * int) * (int * int) * ((int * int) list) =
        match num_times with
        | 0 -> (head, tail, visited)
        | _ -> 
          let (updated_head, updated_tail) = move_head dx dy head tail in
          move_n_times (num_times - 1) dx dy updated_head updated_tail (updated_tail :: visited)
      in
      let (updated_head, updated_tail, visited) = move_n_times (snd command) (fst (fst command)) (snd (fst command)) head tail [] in
      List.append visited (process_inputs rest updated_head updated_tail)
  in
  let visited_tail_positions : (int * int) list = process_inputs input_lines (0, 0) (0, 0) in
  (* de-duplicate visited positions of tail *)
  let unique_visited_tail_positions : (int * int) list = 
    let compare_pairs (x : int * int) (y : int * int) : int = 
      let dx : int = (fst x) - (fst y) in
      let dy : int = (snd x) - (snd y) in
      match dx with
      | 0 -> dy
      | _ -> dx
    in
    List.sort_uniq compare_pairs visited_tail_positions in
  let num_unique_visited_tail_positions : int = List.length unique_visited_tail_positions in
  num_unique_visited_tail_positions;;

let ans = main () in
Printf.printf "answer = %d\n" ans;;

(* 6311 *)
