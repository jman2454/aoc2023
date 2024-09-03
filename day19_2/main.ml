module StringMap = Map.Make(String)
module CharMap = Map.Make(Char)

type comp = Greater | Less | Geq | Leq

(* strings are node labels *)
type node =
| Accept
| Reject
| Branch of char * int * comp * string
| Jump of string

let parse_node s = 
  match String.split_on_char ':' s with 
  | [cond; target] -> 
    let member = String.get cond 0 in 
    let number = String.sub cond 2 (String.length cond - 2) |> int_of_string in
      let comp =
      match String.get cond 1 with 
      | '>' -> Greater
      | '<' -> Less
      | _ -> failwith "oops"
      in
    Branch(member, number, comp, target)
  | [s] -> 
    (match s with 
    | "A" -> Accept
    | "R" -> Reject
    | label -> Jump(label))
  | _ -> failwith "bad node"

let parse_workflow workflow_string map = 
  let list_index = String.index workflow_string '{' in 
  let label = String.sub workflow_string 0 list_index in 
  let instructions = String.sub workflow_string (list_index + 1) (String.length workflow_string - list_index - 2) in 
  StringMap.add label (instructions
  |> String.split_on_char ','
  |> List.map parse_node) map

let parse_program s = 
  let elements = String.split_on_char '\n' s in 
  let rec h lines map = 
    match lines with 
    | [] -> map
    | line::rest -> 
      if line = "" then h rest map else
      if String.get line 0 = '{' then 
        h rest map
      else
        h rest (parse_workflow line map)
  in
  h elements (
    StringMap.empty 
    |> StringMap.add "A" [Accept]
    |> StringMap.add "R" [Reject])

let opposite = function Greater -> Leq | Less -> Geq | Geq -> Less | Leq -> Greater
let bind_constraint n (min,max) = 
  function
  | Geq -> if n > min then (n, max) else (min, max)
  | Leq -> if n < max then (min, n) else (min, max)
  | Greater -> if n + 1 > min then (n + 1, max) else (min, max)
  | Less -> if n - 1 < max then (min, n - 1) else (min, max)

let rec find_constraints nodes workflows curr_constraints constraint_lists = 
  match nodes with 
  | node::rest -> 
    (match node with 
    | Branch (member, n, comp, target) -> 
      let child = StringMap.find target workflows in 
      let (m_min, m_max) = CharMap.find member curr_constraints in 
      find_constraints child workflows (CharMap.add member (bind_constraint n (m_min, m_max) comp) curr_constraints) constraint_lists
      |> find_constraints rest workflows (CharMap.add member (bind_constraint n (m_min, m_max) (opposite comp)) curr_constraints)
    | Jump target -> 
      find_constraints (StringMap.find target workflows) workflows curr_constraints constraint_lists
    | Accept -> curr_constraints::constraint_lists
    | Reject -> constraint_lists)
  | [] -> constraint_lists

let register c map = CharMap.add c (1, 4000) map

let evaluate workflows = 
  let default_map = CharMap.empty |> register 'x' |> register 'm' |> register 'a' |> register 's' in 
  find_constraints (StringMap.find "in" workflows) workflows default_map []

let num_valid map = 
  CharMap.fold (fun _ (min, max) acc -> 
    Int64.mul acc (Int64.add (Int64.sub (max |> Int64.of_int) (min |> Int64.of_int)) Int64.one)) map Int64.one

let sum_valid = List.fold_left (fun acc const -> Int64.add acc (num_valid const)) Int64.zero

let () = 
let constraints = "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"
|> parse_program |> evaluate
in

constraints |> sum_valid |> Int64.to_string |> Printf.printf "total: %s\n"