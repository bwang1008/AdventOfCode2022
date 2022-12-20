(** Run with "$ ocaml day12A.ml" *)

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
  let input_lines : string list = read_lines "inputs/day12.txt" in
  let set_value_in_matrix (matrix : (int option) list list) (row : int) (col : int) (value : int) : (int option) list list = 
    List.mapi (fun index curr_row -> if index = row then 
      (List.mapi (fun index element -> if index = col then Some value else element) curr_row)
    else curr_row) matrix
  in
  let rec find_position_of_char_helper (input_lines : string list) (curr_row : int) (search : char) : (int * int) option = 
    match input_lines with
    | [] -> None
    | line::tail ->
        let get_index_of_char (s : string) (c : char) : int option = 
          let rec helper (s : string) (index : int) : int option = 
            match index with
            | x when x = String.length s -> None
            | _ -> (
                match (String.get s index = c) with
                | true -> Some index
                | false -> helper s (1 + index)
            )
          in
          helper s 0
        in
        match get_index_of_char line search with
        | Some start_col -> Some (curr_row, start_col)
        | None -> find_position_of_char_helper tail (1 + curr_row) search
  in
  let () = Printf.printf "Point A\n" in
  let start_position : (int * int) option = find_position_of_char_helper input_lines 0 'S' in
  let end_position : (int * int) option = find_position_of_char_helper input_lines 0 'E' in
  let height_map : int list list = 
    List.map (fun row -> 
      List.map (fun character -> 
        match character with
        | 'S' -> 0
        | 'E' -> int_of_char 'z' - int_of_char 'a'
        | _ -> int_of_char character - int_of_char 'a'
      )
      (List.init (String.length row) (String.get row))
    )
    input_lines
  in
  let num_rows : int = List.length height_map in
  let num_cols : int = List.length (List.hd height_map) in
  let () = Printf.printf "Point B\n" in
  let rec bfs (queue : (int * int) list) (ans : ((int option) list list)) : ((int option) list list) = 
    (* let () = Printf.printf "IN BFS: first element of queue: %d, %d\n" (fst (List.hd queue)) (snd (List.hd queue)) in *)
    match (List.length queue) with
    | 0 -> ans
    | _ -> 
        let (curr : int * int) = List.hd queue in
        let updated_queue : (int * int) list = List.tl queue in
        let curr_row : int = fst curr in
        let curr_col : int = snd curr in
        (* for each of the 4 neighbors, update their distance if not visited *)
        let add_neighbor (parent_row : int) (parent_col : int) (row : int) (col : int) (queue : (int * int) list) (ans : (int option) list list) : ((int * int) list * (int option) list list) = 
          let () = Printf.printf "Lets add neighbor of %d, %d: %d %d\n" parent_row parent_col row col in
            
          (* neighbor has to be valid index *)
          match (0 <= row && row < num_rows) with
          | false -> (queue, ans)
          | true ->
              match (0 <= col && col < num_cols) with
              | false -> (queue, ans)
              | true -> 
                  (* if visited, don't add to queue *)
                  match (List.nth (List.nth ans row) col) with
                  | Some x -> (queue, ans)
                  | None -> 
                      (* now check edges: can only go at most 1, but can go down many *)
                      match (List.nth (List.nth height_map row) col) - (List.nth (List.nth height_map parent_row) parent_col) <= 1 with
                      | false -> (queue, ans)
                      | true -> 
                          let () = Printf.printf "parent: ans[%d, %d] = \n" parent_row parent_col in
                          let () = Printf.printf "parent: ans[%d, %d] = %d\n" parent_row parent_col (Option.get (List.nth (List.nth ans parent_row) parent_col)) in
                          (List.rev ((row, col) :: List.rev queue), set_value_in_matrix ans row col (1 + Option.get (List.nth (List.nth ans parent_row) parent_col)))
        in
        let (updated_queue, ans) = add_neighbor curr_row curr_col (curr_row - 1) curr_col updated_queue ans in
        let (updated_queue, ans) = add_neighbor curr_row curr_col (curr_row + 1) curr_col updated_queue ans in
        let (updated_queue, ans) = add_neighbor curr_row curr_col curr_row (curr_col - 1) updated_queue ans in
        let (updated_queue, ans) = add_neighbor curr_row curr_col curr_row (curr_col + 1) updated_queue ans in
        bfs updated_queue ans
  in
  let () = Printf.printf "Point C\n" in
  let () = Printf.printf "Start position = " in
  let () = match start_position with
  | Some x -> Printf.printf "(%d, %d)\n" (fst x) (snd x)
  | None -> Printf.printf "No start position\n"
  in
  let () = match end_position with
  | Some x -> Printf.printf "(%d, %d)\n" (fst x) (snd x)
  | None -> Printf.printf "No end position\n"
  in
  let distances : ((int option) list list) option = Option.map (fun starting_position -> 
    let () = Printf.printf "IN first bind. starting_position = (%d, %d)\n" (fst starting_position) (snd starting_position) in
    let empty_matrix : (int option) list list = List.init num_rows (fun col_index -> List.init num_cols (fun index -> None)) in
    let empty_matrix : (int option) list list = set_value_in_matrix empty_matrix (fst (Option.get start_position)) (snd (Option.get start_position)) 0 in
    let () = Printf.printf "Ok empty matrix size: %d x %d\n" (List.length empty_matrix) (List.length (List.hd empty_matrix)) in
    bfs [starting_position] empty_matrix
  ) start_position
  in
  let () = Printf.printf "Point D\n" in
  let ans : int option = Option.bind distances (fun distances -> Option.bind end_position (fun end_position -> List.nth (List.nth distances (fst end_position)) (snd end_position))) in
  let () = Printf.printf "Point E\n" in
  ans;;

let ans : int option = main ();;
match ans with
| Some ans -> Printf.printf "answer = %d\n" ans
| None -> Printf.printf "That's not good\n"
;;

(* 361 *)
