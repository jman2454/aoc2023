let input = "Time:        48     93     85     95
Distance:   296   1928   1236   1391"

let list_to_twople lst = 
  match lst with 
  | a::b::[] -> (a,b)
  | _ -> failwith "Invalid twople list!"

let (times, dists) = 
  list_to_twople (
    String.split_on_char '\n' (String.trim input) 
    |> List.filter ((<>) "") 
    |> List.map String.trim
    |> List.filter ((<>) "") 
    |> List.map 
        (fun s -> 
          List.nth (String.split_on_char ':' s) 1
          |> String.split_on_char ' '
          |> List.fold_left (fun acc s -> acc ^ s) ""
          |> float_of_string))

let solve t d = 
  let a = -1.0 in 
  let b = t in 
  let c = Float.neg d in
  let b2 = Float.pow b 2.0 in
  let negb2 = Float.neg b2 in
  let sqrtterm = Float.sqrt (Float.sub b2 (Float.mul 4.0 (Float.mul a c))) in
  let twoa = Float.mul a 2.0 in
  let compose f s d = Float.div (Float.sub f s) d in
  let (s1, s2) = (compose negb2 sqrtterm twoa, compose negb2 (Float.neg sqrtterm) twoa) in
  (Float.floor s2 |> Int.of_float) - (Float.ceil s1 |> Int.of_float) + 1 |> abs

let races = List.combine [times] [dists]
let result = List.fold_left (fun acc (t, d) -> acc * (solve t d)) 1 races

let () = Printf.printf "%d\n" result