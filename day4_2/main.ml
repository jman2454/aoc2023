module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

let (>>) f g = fun x -> g (f x)
let flip f = fun a b -> f b a

let parse_int_list = 
  String.split_on_char ' ' 
  >> List.filter ((=) ' ' |> String.for_all >> not)
  >> List.map (String.trim >> int_of_string)

let list_to_twople lst = 
  match lst with 
  | a::b::[] -> (a,b)
  | _ -> failwith "Invalid twople list!"

let parse_twople c = String.split_on_char c >> list_to_twople
let parse_int_lists = String.split_on_char '|' >> List.map parse_int_list >> list_to_twople
let parse_card card = String.sub card 4 (String.length card - 4) |> String.trim |> int_of_string
let get_default key map default = IntMap.find_opt key map |> Option.value ~default:default

let get_game_score (map, prev_mult, score) line = 
  let (card_s, values) = parse_twople ':' line in
  let card = parse_card card_s in
  let (winners, drawn) = parse_int_lists values in
  let winners = List.fold_left (flip IntSet.add) IntSet.empty winners in
  let winner_count = List.fold_left (fun count num -> if IntSet.mem num winners then count + 1 else count) 0 drawn in
  let next_delta = get_default (card + 1) map 0 in
  let end_delta = get_default (card + 1 + winner_count) map 0 in
  let mult = get_default card map 0 |> (+) prev_mult in
  let new_map = IntMap.remove card map in
  if winner_count > 0 then
    (IntMap.add (card + 1) (next_delta + mult) new_map |> IntMap.add (card + 1 + winner_count) (end_delta - mult), mult, score + mult)
  else
    (new_map, mult, score + mult)

let input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

let (_, _, final) = List.fold_left get_game_score (IntMap.empty, 1, 0) (String.split_on_char '\n' input)
let () = Printf.printf "%d\n" final