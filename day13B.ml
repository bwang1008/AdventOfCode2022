(** Run with "$ ocaml day13B.ml" *)

(** function to read input from file copied from https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)

(* Read in lines from a text file *)
let read_lines file_name : string list = 
    let ic : in_channel = open_in file_name in
    let try_read () : string option = 
        try
            Some (input_line ic)
        with
            End_of_file -> None
    in
    let rec loop (accumulated : string list) : string list = 
        match try_read () with
        | Some s -> loop (s :: accumulated)
        | None -> 
            let () = close_in ic in
            List.rev accumulated
    in
    loop [];;

(* given a string and an index of a "[", find the index of the matching "]"*)
let find_closing_bracket (str : string) (open_index : int) : int =
    let rec find_closing_bracket_helper (str : string) (curr_index : int) (additional_open_brackets : int) : int = 
        match String.get str curr_index with
        | ']' -> (
            match additional_open_brackets - 1 with
            | 0 -> curr_index
            | _ -> find_closing_bracket_helper str (curr_index + 1) (additional_open_brackets - 1)
            )
        | '[' -> (
            find_closing_bracket_helper str (curr_index + 1) (additional_open_brackets + 1)

        )
        | _ -> (
            find_closing_bracket_helper str (curr_index + 1) additional_open_brackets
        )
    in
    find_closing_bracket_helper str open_index 0
;;

(* convert sth like "[1,[2]]" into ["1", "[2]"] *)
let string_to_list (str : string) : string list =
    let rec string_to_list_helper (str : string) (curr_index : int) (accumulate : string list) : string list = 
        match (curr_index >= String.length str) with
        | true -> List.rev accumulate
        | false -> (
            match String.get str curr_index with
            | '[' -> (
                let close_index : int = find_closing_bracket str curr_index in
                string_to_list_helper str (1 + close_index) ((String.sub str curr_index (close_index - curr_index + 1)) :: (List.tl accumulate))
            )
            | ',' -> string_to_list_helper str (1 + curr_index) ("" :: accumulate)
            | _ -> (
                let updated_element : string = String.cat (List.hd accumulate) (String.sub str curr_index 1) in
                string_to_list_helper str (1 + curr_index) (updated_element :: (List.tl accumulate))
            )
        )
    in
    match (str = "[]") with
    | true -> []
    | false -> (
        (* strip off first [ and last ], add beginning comma *)
        let removed_surroundings : string = String.sub str 1 ((String.length str) - 2) in
        let prepended_first_element_with_comma : string = String.cat "," removed_surroundings
        in
        string_to_list_helper prepended_first_element_with_comma 0 []
    )
;;

(* comparison function to sort list of strings *)
let rec compare (line_a: string) (line_b: string) : int =
    (* Printf.printf "\n--------------\ncompare line_a = %s    v    %s\n" line_a line_b; *)
    (* helper function to compare list of strings, by comparing lengths and each element recursively *)
    let compare_lists (list_a : string list) (list_b : string list) : int = 
        let rec compare_lists_helper (list_a : string list) (list_b : string list) (index : int) : int =
            (* Printf.printf "compare_lists_helper:\n"; *)
            (* List.iteri (Printf.printf "list_a[%d] = %s\n") list_a; *)
            (* Printf.printf "v\n"; *)
            (* List.iteri (Printf.printf "list_b[%d] = %s\n") list_b; *)
            (* check out of bounds *)
            match ((List.length list_a - index), (List.length list_b - index)) with
            | (0, 0) -> 0
            | (0, _) -> -1
            | (_, 0) -> 1
            | (_, _) -> (
                let element_comparison : int = compare (List.nth list_a index) (List.nth list_b index) in
                match element_comparison with
                | 0 -> compare_lists_helper list_a list_b (1 + index)
                | _ when (element_comparison > 0) -> 1
                | _ -> -1
            )
        in
        compare_lists_helper list_a list_b 0
    in
    let compare_ints (a : int) (b : int) : int = a - b in
    let compare_list_with_nonlist (a : string list) (b : string) : int =
        compare_lists a [b]
    in
    let is_list (str : string) : bool = (String.length str > 0) && (String.get str 0 = '[')
    in
    match ((is_list line_a), (is_list line_b)) with
    | true, true -> compare_lists (string_to_list line_a) (string_to_list line_b)
    | true, false -> compare_list_with_nonlist (string_to_list line_a) line_b
    | false, true -> - (compare_list_with_nonlist (string_to_list line_b) line_a)
    | false, false -> compare_ints (int_of_string line_a) (int_of_string line_b)
;;


let main : int =
    let input_lines : string list = read_lines "inputs/day13.txt" in
    (* let input_lines : string list = read_lines "inputs/dummy.txt" in *)
    let input_lines : string list = List.filter (fun x -> (String.length x > 0)) input_lines in
    let input_lines : string list = "[[6]]" :: "[[2]]" :: input_lines in
    List.iteri (Printf.printf "list[%d] = %s\n%!") input_lines;
    let sorted_input_lines : string list = List.sort compare input_lines in
    Printf.printf "\n------------- Sorted! -------------\n";
    List.iteri (Printf.printf "sorted_list[%d] = %s\n%!") sorted_input_lines;
    let get_index list_a item : int =
        let rec get_index_helper list_a item (index : int) : int =
            match (List.nth list_a index) = item with
            | true -> index
            | false -> get_index_helper list_a item (1 + index)
        in
        get_index_helper list_a item 0
    in
    (* convert 0-index to 1-index *)
    let marker_1 : int = 1 + (get_index sorted_input_lines "[[2]]") in
    let marker_2 : int = 1 + (get_index sorted_input_lines "[[6]]") in
    marker_1 * marker_2
;;


let ans : int = main in
Printf.printf "answer = %d\n%!" ans;;

(* 22134 *)
