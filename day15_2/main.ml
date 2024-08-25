open Aoc.Public

let hash s = Seq.fold_left (fun acc c -> (17 * (acc + (Char.code c))) mod 256) 0 s
let instructions s = String.split_on_char ',' s
let hide_newlines s = String.to_seq s |> Seq.filter ((<>) '\n')
let (@) a b = (fun x -> b (a x))
let no_newline_hash = hide_newlines @ hash

type 'a linked_list = Node of 'a node | Nil and 'a node = { value : 'a ref; left : 'a linked_list ref; right : 'a linked_list ref }

let rec find_mapped value map cmp = function 
  | Node n -> 
    if cmp value (map !(n.value)) then Some n else find_mapped value map cmp !(n.right)
  | Nil -> None

let rec head = function 
| Node n as l -> if !(n.left) = Nil then l else head !(n.left)
| Nil -> Nil

let remove_mapped value map cmp ll =
  match find_mapped value map cmp ll with 
  | Some n -> 
    (match !(n.left), !(n.right) with 
    | Nil, Nil -> Nil
    | Nil, Node right -> right.left := Nil; Node right
    | Node left, Nil -> left.right := Nil; head (Node left)
    | Node left, Node right as tup -> 
      let (l, r) = tup in
      left.right := r;
      right.left := l;
      head (Node left))
  | None -> ll

let rec tail = function 
| Node n when !(n.right) <> Nil -> tail !(n.right) 
| ll -> ll

let put slots key value = 
  let index = (no_newline_hash key) mod 256 in
  match find_mapped key (fun (k, _) -> k) (=) (slots --> index) with 
  | Some n -> n.value := (key, value); slots
  | None -> 
    match slots --> index |> tail with 
    | Nil -> (slots, index) <-- Node({ value = ref (key, value); right = ref Nil; left = ref Nil })
    | Node n as l -> n.right := Node({ value = ref (key, value); right = ref Nil; left = ref l; }); 
    slots

let remove slots key = 
  let index = (no_newline_hash key) mod 256 in
  (slots, index) <-- remove_mapped key (fun (k, _) -> k) (=) (slots --> index)

let slots = Pvector.make_vec 256 Nil

let ll_foldi fn acc ll = 
  let rec h i acc = function
  | Nil -> acc
  | Node n -> h (i + 1) (fn i acc !(n.value)) !(n.right)
  in
  h 0 acc ll

let sum_boxes = 
  Pvector.fold_lefti (fun acc i_box ll -> 
      ll_foldi (fun i_lens acc (_, focal_length) ->
         acc + (1 + i_lens) * (1 + i_box) * focal_length) acc ll
  ) 0

let execute_instruction s slots =   
  if String.get s (String.length s - 1) = '-' then 
    remove slots @@ String.sub s 0 (String.length s - 1)
  else
    match String.split_on_char '=' s with 
    | [key; value] -> put slots key (int_of_string value)
    | _ -> failwith "wow!"


let () = 
let insts = instructions "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" in
let result = List.fold_left (fun slots instruction -> execute_instruction instruction slots) slots insts in 
result
|> sum_boxes
|> Printf.printf "Score: %d\n"