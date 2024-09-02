let instructions str =
  String.split_on_char '\n' str 
  |> List.map (fun s -> 
    match String.split_on_char ' ' s with 
    | [dir; amt; _] -> (String.get dir 0, int_of_string amt)
    | _ -> failwith "bad instruction")

let walk_inst (x, y) (dir, amt) = 
  match dir with 
  | 'L' -> (x - amt, y)
  | 'R' -> (x + amt, y)
  | 'U' -> (x, y + amt)
  | 'D' -> (x, y - amt)
  | _ -> failwith "bad instruction"

let get_perim_and_verts insts = 
  let (_, perim), verts = List.fold_left_map (fun (pos, perim) inst -> let new_pos = walk_inst pos inst in (new_pos, perim + snd inst), new_pos) ((0, 0), 0) insts
  in perim, verts

let fold_pt_pairs verts fn acc = 
  List.fold_left (fun (prev_opt, acc) nxt ->
    match prev_opt with 
    | None -> Some(nxt), acc
    | Some prev -> Some(nxt), acc + fn prev nxt
  ) (None, acc) verts
  |> snd

let compute_area verts = 
  fold_pt_pairs verts (fun (x1, y1) (x2, y2) -> x1 * y2 - x2 * y1) 0
  |> abs
  |> fun x -> x / 2

let part1 insts = 
  let perim, verts = insts |> instructions |> get_perim_and_verts in 
  compute_area verts + perim / 2 + 1

let () =
"R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"
|> part1
|> Printf.printf "area: %d\n"