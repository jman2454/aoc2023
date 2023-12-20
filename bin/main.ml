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

let c_fold_left fn s init = c_fold fn s init true
let c_fold_right fn s init = c_fold fn s init false

type match_status = 
  | Match
  | Partial of int

type match_buffer = string * int * match_status

let step_buffer (mb : match_buffer) c = 
  let (s, result, buf) = mb in
  let new_status = 
    match buf with 
    | Match -> Match
    | Partial index ->
      if s.[index] <> c then 
        (if s.[0] <> c then 
          Partial 0
        else Partial 1)
      else
        if index == String.length s - 1 then
          Match
        else
          Partial (index + 1)
    in (s, result, new_status)

let compose_bufs buf1 buf2 = 
  let (_, r, status) = buf2 in
    match buf1 with 
    | Some x -> Some x
    | None -> 
      match status with 
      | Match -> Some r
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

(* let inputs = String.split_on_char '\n' "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen" *)

let buffers = List.map (fun tup -> let (s, v) = tup in (s, v, Partial 0)) entries

type maybe_match = Partial of match_buffer list | Match of int
let get_result res = 
  match res with 
  | Match i -> i
  | _ -> 0

let extract_number s forward = c_fold (fun acc c -> 
  match acc with 
  | Match i -> Match i
  | Partial lst -> 
    let (result, new_lst) = 
      List.fold_left_map (fun acc buf -> let new_buf = step_buffer buf c in (compose_bufs acc new_buf, new_buf)) None lst 
    in 
      match result with 
      | None -> Partial new_lst
      | Some v -> Match v
) s (Partial buffers) forward |> get_result

let l = c_fold_right (fun acc nxt -> nxt::acc) "one" []
let l2 = c_fold_left (fun acc nxt -> nxt::acc) "one" []

let () = 
  List.iter (Printf.printf "%c ") l2;
  List.iter (Printf.printf "%c ") l;
  Printf.printf "%d" (extract_number "ontwosone" false);
  