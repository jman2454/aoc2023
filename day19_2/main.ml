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


let rec find_constraints nodes workflows curr_constraints constraint_lists = 
  match nodes with 
  | node::rest -> 
    (match node with 
    | Branch (member, n, comp, target) -> 
      let child = StringMap.find target workflows in 
      find_constraints child workflows ((member, n, comp)::curr_constraints) constraint_lists
      |> find_constraints rest workflows ((member, n, opposite comp)::curr_constraints)
    | Jump target -> 
      find_constraints (StringMap.find target workflows) workflows curr_constraints constraint_lists
    | Accept -> curr_constraints::constraint_lists
    | Reject -> constraint_lists)
  | [] -> constraint_lists

let evaluate workflows = find_constraints (StringMap.find "in" workflows) workflows [] []

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
{x=2127,m=1623,a=2188,s=1013}" |> parse_program |> evaluate
in

let string_of_comp = function | Geq -> "Geq" | Leq -> "Leq" | Greater -> "Greater" | Less -> "Less" in 
List.iter (fun constraint_list -> 
  Printf.printf "NEXT:\n";
  List.iter (fun (member, n, comp) -> Printf.printf "\t%c %s %d\n" member (string_of_comp comp) n) constraint_list)
constraints