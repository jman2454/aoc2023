include Aoc

module Pvector = Aoc.Pvector

let (-->) = Pvector.(-->)
let (<--) = Pvector.(<--)

let parse line = 
  String.split_on_char ' ' line
  |> function 
  | [a; b] -> Pvector.of_string a, String.split_on_char ',' b |> List.map int_of_string
  | _ -> failwith "invalid"

let branch slot_vec start count = 
  let s_len = Pvector.len slot_vec in
  let rec h i ct locked vec acc = 
    if ct = 0 then 
      (
        if i < Pvector.len vec && vec --> i = '#' then vec, acc else 
          vec, Pvector.append (i + 1, vec) acc
      )
    else 
    if i >= s_len then vec, acc else (* the > will handle bad inputs *)
    let c = vec --> i in 
    if locked && c = '.' then vec, acc 
    else if c = '.' then h (i + 1) count false vec acc 
    else if c = '#' then h (i + 1) (ct - 1) true vec acc
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
        Pvector.fold_left (fun sum (start, slots) -> 
          sum + h start slots rest acc (index + 1)) 0 sub_slots
      )
    | [] -> 
      if Pvector.anyi (fun i c -> i >= start && c = '#') slots then (acc) else
        1 + acc
    in
    h 0 slots counts 0 0

let input = "#..#??# 1,2"

let () = 
  String.split_on_char '\n' input
  |> List.fold_left (fun acc line -> acc + arrangements line) 0
  |> Printf.printf "ANSWER: %d\n";