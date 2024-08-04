include Aoc

module Pvector = Aoc.Pvector

let (-->) = Pvector.(-->)
let (<--) = Pvector.(<--)

let parse line = 
  String.split_on_char ' ' line
  |> function 
  | [a; b] -> Pvector.of_string a, String.split_on_char ',' b |> List.map int_of_string
  | _ -> failwith "invalid"

(* number of ways we can assign ?s in <slots> s.t. the first 
  contiguous string of #s (after subbing) has length <count> *)
(* let n_ways slots count = 
  let s_len = String.length slots in 
  let rec h i ct acc locked = 
    if ct = 0 then 1 + acc else
    if i >= s_len then acc else (* the > will handle bad inputs *)
    let c = String.get slots i in 
    if c = '.' then h (i + 1) count 0 false else 
    if c = '#' then h (i + 1) (ct - 1) acc true else
    h (i + 1) (ct - 1) (if locked then acc else h (i + 1) ct acc false) locked
  in
  h 0 count 0 false *)

(* let transparent_print el_to_str vec = 
  Printf.printf "VEC: %s\n" @@ Pvector.to_str el_to_str vec;
  vec *)

let branch slot_vec start count = 
  let s_len = Pvector.len slot_vec in
  let rec h i ct locked vec acc = 
    (* Printf.printf "Visiting index %d\n" i; *)
    if ct = 0 then 
      (
        (* Printf.printf "i: %d\n" i;
        let vv = 
          Pvector.mapi (fun o c -> if o <= i then '.' else c) vec
          |> transparent_print (String.make 1)
          |> Pvector.any ((=) '#')
        in *)

        (* if i < Pvector.len vec && vv then vec, acc else *)
        if i < Pvector.len vec && vec --> i = '#' then vec, acc else 
        (* if index = 2 then Printf.printf "appending at %d\n" i else (); *)
          vec, Pvector.append (i + 1, vec) acc
      )
    else 
    if i >= s_len then vec, acc else (* the > will handle bad inputs *)
    let c = vec --> i in 
    if locked && c = '.' then vec, acc 
    else if c = '.' then h (i + 1) count false vec acc 
    (* else if locked && c = '#' then h (i + 1) ct  *)
    else if c = '#' then
      (
        (* Printf.printf "Starting at ind %d looking for %d\n" i ct; *)
      h (i + 1) (ct - 1) true vec acc )
      (* (let sub, acc =  *)
        (* h (i + 1) (ct - 1) true vec acc  *)
      (* in 
      if locked then sub, acc else h (i + 1) ct false vec acc) *)
    else
    let _, acc = if locked then vec, acc else h (i + 1) ct false vec acc in 
    h (i + 1) (ct - 1) true ((vec, i) <-- '#') acc
  in
  let _, ans = h start count false slot_vec Pvector.empty in 
  ans

let arrangements line = 
  let (slots, counts) = parse line in 
  let rec h start slots cts acc index = 
    match cts with 
    | ct::rest -> 
      (
        let sub_slots = branch slots start ct in 

        (* Printf.printf "ct: %d, start: %d, slots: %s\n" ct start @@ Pvector.to_str (fun c -> String.make 1 c) slots; *)
        Printf.printf "ct_i: %d result: %s\n" index (sub_slots |> Pvector.to_str (fun (start, pvec) -> string_of_int start ^ ", " ^ Pvector.to_str (fun c -> String.make 1 c) pvec));
        Pvector.fold_left (fun sum (start, slots) -> 
          sum + h start slots rest acc (index + 1)) 0 sub_slots
      )
    | [] -> 

      let vv = 
        Pvector.mapi (fun o c -> if o <= start then '.' else c) slots
        (* |> transparent_print (String.make 1) *)
        |> Pvector.any ((=) '#')
      in
      if vv then (Printf.printf "discarding;\n"; acc) else
      (* Printf.printf "%d\n" start; *)
        1 + acc
    in
    h 0 slots counts 0 0

let input = "?????? 1,2"

(* one, one, one *)
(* ??#??..# *)
(* #.#....# *)
(* ..#.#..# *)

let () = 
  String.split_on_char '\n' input
  |> List.fold_left (fun acc line -> acc + arrangements line) 0
  |> Printf.printf "ANSWER: %d\n";
(* n_ways "???.###" 1 |> Printf.printf "%d\n";
branch (Pvector.of_string "???.###") 7 1
|> Pvector.to_str (fun (start, pvec) -> string_of_int start ^ ", " ^ Pvector.to_str (fun c -> String.make 1 c) pvec) 
|> Printf.printf "%s\n"; *)

(* let inp = ".??..??...?##. 1,1,3" in
(* let inp = "????.######..#####. 1,6,5" in  *)
let ans = arrangements inp in 
Printf.printf "Arrangements for \"%s\": %d\n" inp ans *)