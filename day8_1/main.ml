module StringMap = Map.Make(String)
module Buffer = Aoc.Circularbuffer

let parse_map lines = 
  List.fold_left (
    fun map line -> 
      match String.split_on_char '=' line with 
      | l::r::[] -> 
          (match String.split_on_char ',' r with 
          | k1::k2::[] -> 
              let cl = String.sub k1 2 (String.length k1 - 2) in
              let cr = String.sub k2 1 (String.length k1 - 2) in
              StringMap.add (String.trim l) (cl |> String.trim, cr |> String.trim) map
          | _ -> failwith "bad!")
      | _ -> failwith "bad!"
    ) 
    StringMap.empty lines

let parse_seq s = String.fold_right (fun c l -> c::l) s [] |> Buffer.of_list

(* we could have O(1) neighbor lookup with some mutable structures, but I don't wanna! *)
let rec find_zzz start buf map count = 
  match start with 
  | "ZZZ" -> count
  | _ -> 
    let (l, r) = StringMap.find start map in
    let c = match Buffer.get_value buf with 
    | 'L' -> l
    | 'R' -> r
    | _ -> failwith "oops!" in 
    find_zzz c (Buffer.next buf) map (count + 1)

let get_result input = 
  match String.split_on_char '\n' input with 
  | insts::rest -> 
    let buf = parse_seq insts in 
    let map = parse_map (rest |> List.filter ((<>) "")) in 
    find_zzz "AAA" buf map 0
  | _ -> failwith "bad!"

let input = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"

let () = Printf.printf "%d\n" @@ get_result input;