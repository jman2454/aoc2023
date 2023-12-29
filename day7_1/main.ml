module CharMap = Map.Make(Char)
module IntMap = Map.Make(Int)

(* fn composition *)
let (>>) g f = fun x -> f (g x)
let id a = a

let list_to_twople_p parse_first parse_second lst = 
  match lst with 
  | a::b::[] -> (parse_first a, parse_second b)
  | _ -> failwith "Invalid twople list!"

(* should functor this but I'm lazy *)
let get_default_c map key default = Option.value (CharMap.find_opt key map) ~default:default
let get_default_i map key default = Option.value (IntMap.find_opt key map) ~default:default

let list_of_string s = String.fold_right (fun c acc -> c::acc) s []
let cards = list_of_string "23456789TJQKA"

let (card_strengths, _) = 
  List.fold_left (fun (map, pos) c -> (CharMap.add c pos map, pos + 1)) 
  (CharMap.empty, 0) 
  cards

let non_high_card_min = CharMap.find (List.rev cards |> List.hd) card_strengths + 1
let map_inverse map = 
  CharMap.fold (fun c count revved -> IntMap.add count (get_default_i revved count [] |> List.cons c) revved) map IntMap.empty

let hand_strength hand = 
  let (c_to_count, high_card) = String.fold_left (fun (map, high) c -> 
    (CharMap.add c (get_default_c map c 0 + 1) map, Int.max (CharMap.find c card_strengths) high))
    (CharMap.empty, -1) hand in
  let counts = map_inverse c_to_count in 
  match IntMap.max_binding counts with 
  | (5, _) -> non_high_card_min + 5
  | (4, _) -> non_high_card_min + 4
  | (3, _) -> non_high_card_min + if IntMap.mem 2 counts then 3 else 2
  | (2, l) -> non_high_card_min + if List.length l > 1 then 1 else 0
  | _ -> high_card

let strengths hand = List.map (fun c -> CharMap.find c card_strengths) (hand |> list_of_string)

let compare_hands h1 s1 h2 s2 = 
  if s1 = s2 then 
    List.compare Int.compare (strengths h1) (strengths h2)
  else
    Int.compare s1 s2

let parse_bets input = 
  String.split_on_char '\n' input 
  |> List.filter ((<>) "") 
  |> List.map (String.split_on_char ' ' >> list_to_twople_p id int_of_string)

let with_score bets = List.map (fun (h, b) -> (h, b, hand_strength h)) bets

let sort_bets bets = 
  List.sort (fun (h1, _, s1) (h2, _, s2) -> compare_hands h1 s1 h2 s2) bets

let score bets = List.mapi (fun i (_, b, _) -> (i+1) * b) bets |> List.fold_left (+) 0

let input = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"

let result = parse_bets input |> with_score |> sort_bets |> score
let () = Printf.printf "%d\n" result