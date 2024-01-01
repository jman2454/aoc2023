(* fn composition *)
let (>>) g f = fun x -> f (g x)

let parse_int_list s = 
  String.split_on_char ' ' s
  |> List.filter ((=) ' ' |> String.for_all >> not)
  |> List.map (fun s -> String.trim s |> int_of_string)

let rec print_int_list list =
  match list with 
  | [] -> Printf.printf "%!"
  | a::[] -> Printf.printf "%d\n%!" a
  | a::rest -> Printf.printf "%d, " a; print_int_list rest