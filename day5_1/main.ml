module IntMap = Map.Make(Int)

(* fn composition *)
let (>>) g f = fun x -> f (g x)

(* get threeple or throw *)
let list_to_threeple lst = 
  match lst with 
  | a::b::c::[] -> (a,b,c)
  | _ -> failwith "Invalid threeple list!"

(* parses a sep-separated int list from a string *)
let int_list_of_string ?(sep = ' ') = 
  String.split_on_char sep
  >> List.map (String.trim >> int_of_string_opt >> Option.to_list)
  >> List.flatten

let input = "seeds: 79 14 55 13
seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"

(* splits on colons into newline-delineated sections *)
let get_sections = String.split_on_char ':' >> List.map (String.split_on_char '\n')

(* turns a list of strings of 3 ' '-separated numbers into a map from the second column to the (first, third) *)
let parse_map (lst : string list) =
  let int_lists = (List.filter ((<>) []) (List.map int_list_of_string lst)) in
  let ils = List.map (fun l -> list_to_threeple l) int_lists in
  List.fold_left (fun mp (dest, src, ln) -> IntMap.add src (dest, ln) mp) IntMap.empty ils

(* binary search: Some target if target is in the list, and depending on geq, Some v where v is either the min_ge or max_le. 
   None if no suitable result *)
let map_search_opt map target geq =
  let (lt, eq, gt) = IntMap.split target map in
  match eq with 
  | Some v -> Some (target, v)
  | None -> 
    if geq then 
      IntMap.min_binding_opt gt
    else 
      IntMap.max_binding_opt lt

let get_mapped_value map x = 
  Option.bind (map_search_opt map x false) (fun (k, (v, r)) -> if x < k + r then Some (x - k + v) else None) |> Option.value ~default:x

let parse_input input = 
  match get_sections input with 
  | _::seeds::maps -> 
    let seed_vals = List.map int_list_of_string seeds |> List.flatten in 
    let map_vals = Seq.map parse_map (List.to_seq maps) |> List.of_seq in 
    List.fold_left (fun min seed -> Int.min (List.fold_left (fun res map -> get_mapped_value map res) seed map_vals) min) Int.max_int seed_vals
  | _ -> failwith "bad input [ or parsing :) ]!"

let () = Printf.printf "%d\n" (parse_input input)