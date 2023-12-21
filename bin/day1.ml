let c_fold fn s init forward = 
  let start = if forward then 0 else String.length s - 1 in
  let in_range = if forward then fun x -> x < String.length s - 1 else fun x -> x > 0 in
  let next_ind = if forward then fun x -> x + 1 else fun x -> x - 1 in
  let rec c_fold_helper ind = 
    if in_range ind then
      fn (ind |> next_ind |> c_fold_helper) s.[ind]
    else
      fn init s.[ind]
  in c_fold_helper start

type search_status = 
  | FoundResult
  | SearchIndex of int

(* let x = Option.bind *)

type match_buffer = string * int * search_status

let step_buffer (mb : match_buffer) c forward = 
  let (pattern, parsed_value, search_pos) = mb in
  let (start, finish, get_next) = 
    if forward 
    then (0, String.length pattern - 1, fun x -> x + 1)
    else (String.length pattern - 1, 0, fun x -> x - 1)
  in
  let new_status = 
    match search_pos with 
    | FoundResult -> FoundResult
    | SearchIndex index ->
      if pattern.[index] <> c then 
        (if pattern.[start] <> c then 
          SearchIndex start
        else SearchIndex (get_next start))
      else
        if index == finish then
          FoundResult
        else
          SearchIndex (get_next index)
    in (pattern, parsed_value, new_status)

let compose_bufs buf1 buf2 = 
  let (_, r, status) = buf2 in
    match buf1 with 
    | Some x -> Some x
    | None -> 
      match status with 
      | FoundResult -> Some r
      | _ -> None

(***
  just need multiple 'match buffers', one for each substring. 
  if any of them match at any time, we're done. 
***)
let entries = [
  ("one",1);("two",2);("three",3);("four",4);
  ("five",5);("six",6);("seven",7);("eight",8);("nine",9);
  ("1",1);("2",2);("3",3);("4",4);("5",5);("6",6);("7",7);
  ("8",8);("9",9)
]

let get_buffers forward = List.map (fun tup -> let (s, v) = tup in (s, v, SearchIndex (if forward then 0 else String.length s - 1))) entries

type maybe_match = Searches of match_buffer list | Parsed of int
let get_result res = 
  match res with 
  | Parsed i -> i
  | _ -> 0

let extract_number s forward = c_fold (fun buffers c -> 
  match buffers with 
  | Parsed i -> Parsed i
  | Searches lst -> 
    let (found_match, stepped_list) = 
      List.fold_left_map (fun acc buf -> let new_buf = step_buffer buf c (not forward) in (compose_bufs acc new_buf, new_buf)) None lst 
    in 
      match found_match with 
      | None -> Searches stepped_list
      | Some v -> Parsed v
) s (Searches (get_buffers (not forward))) forward |> get_result

let inputs = String.split_on_char '\n' "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"

let results = List.map (fun s -> (extract_number s false) * 10 + (extract_number s true)) inputs
let final = List.fold_left (fun x y -> x + y) 0 results

let () = 
  Printf.printf "%d\n" final;
  