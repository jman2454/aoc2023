module IntMap = Map.Make(Int)

let game_maxes game_set = 
  let rounds = 
    String.split_on_char ';' game_set 
    |> List.map (fun round -> String.trim round |> String.split_on_char ',') 
    |> List.flatten 
  in
    List.fold_left (fun map s -> 
        let (r,g,b) = map in 
        match String.split_on_char ' ' (String.trim s) with
        | count_str::color::[] -> 
          let count = int_of_string count_str in 
          (match color with 
          | "red" -> Int.max r count, g, b
          | "green" -> r, Int.max g count, b
          | "blue" -> r, g, Int.max b count
          | _ -> map)
        | _ -> map) (0, 0, 0) rounds
        
let parse_round counts line = 
  match String.split_on_char ':' line with
  | g_n::game::[] -> 
    let game_number = String.sub g_n 5 (String.length g_n - 5) |> int_of_string in
    let (r,g,b) = game_maxes game in 
    let (r', g', b') = match IntMap.find_opt game_number counts with | None -> (0,0,0) | Some tup -> tup in
      IntMap.add game_number (Int.max r r', Int.max g g', Int.max b b') counts
  | _ -> failwith "Oops"

(* let valid_game r g b = r <= 12 && g <= 13 && b <= 14 *)

let lines = String.split_on_char '\n' "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

let games_map = List.fold_left parse_round IntMap.empty lines
(* let valid_games = IntMap.filter (fun _ (r,g,b) -> valid_game r g b) games_map *)

let sum = IntMap.fold (fun _ (r,g,b) sum -> (r * g * b) + sum) games_map 0

let () = Printf.printf "%d\n" sum