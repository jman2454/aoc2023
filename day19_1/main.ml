module StringMap = Map.Make(String)

type part = { x : int; m : int; a : int; s : int }

(* strings are node labels *)
type node =
| Accept
| Reject
| Branch of (part -> bool) * string
| Jump of string

let get_accessor =
  function
  | 'x' -> fun part -> part.x
  | 'm' -> fun part -> part.m
  | 'a' -> fun part -> part.a
  | 's' -> fun part -> part.s
  | _ -> failwith "bad member"

let parse_node s = 
  match String.split_on_char ':' s with 
  | [cond; target] -> 
    let accessor = get_accessor (String.get cond 0) in 
    let number = String.sub cond 2 (String.length cond - 2) |> int_of_string in
      let matcher =
      match String.get cond 1 with 
      | '>' -> fun p -> accessor p > number
      | '<' -> fun p -> accessor p < number
      | _ -> failwith "oops"
      in
    Branch(matcher, target)
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

let parse_part node_string = 
  let rec h assignment_list part = 
    match assignment_list with 
    | assignment::rest -> 
      let c = String.get assignment 0 in 
      let number = String.sub assignment 2 (String.length assignment - 2) |> int_of_string in 
      let part = 
        match c with 
        | 'x' ->  { part with x = number }
        | 'm' ->  { part with m = number }
        | 'a' ->  { part with a = number }
        | 's' ->  { part with s = number }
        | _ -> failwith "bad assignment"
      in
      h rest part
    | [] -> part
  in
  h (String.sub node_string 1 (String.length node_string - 2) |> String.split_on_char ',') { x=0;m=0;a=0;s=0; }
  
let parse_program s = 
  let elements = String.split_on_char '\n' s in 
  let rec h lines (map, parts) = 
    match lines with 
    | [] -> (map, parts)
    | line::rest -> 
      if line = "" then h rest (map, parts) else
      if String.get line 0 = '{' then 
        h rest (map, parse_part line::parts)
      else
        h rest (parse_workflow line map, parts)
  in
  h elements (
    StringMap.empty 
    |> StringMap.add "A" [Accept]
    |> StringMap.add "R" [Reject], [])

type state = 
 | Stop of bool
 | Continue

let rec eval_node part node workflows = 
  match node with 
  | Branch (cond, target) -> 
    if cond part then eval_workflow part (StringMap.find target workflows) workflows else Continue
  | Jump target -> 
    eval_workflow part (StringMap.find target workflows) workflows
  | Accept -> Stop(true)
  | Reject -> Stop(false)

and eval_workflow part wf workflows = 
  match wf with 
  | [] -> failwith "wow!"
  | node::rest -> 
    match eval_node part node workflows with 
    | Stop(true) -> Stop(true)
    | Stop(false) -> Stop(false)
    | Continue -> eval_workflow part rest workflows

let evaluate part workflows = 
  eval_workflow part (StringMap.find "in" workflows) workflows

let evaluate_program (map, parts) = 
  List.filter (fun part -> 
    match evaluate part map with 
    | Stop(b) -> b
    | _ -> failwith "wow") parts
  |> List.fold_left (fun acc part -> acc + part.x + part.m + part.a + part.s) 0

let () = 
"px{a<2006:qkq,m>2090:A,rfg}
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
{x=2127,m=1623,a=2188,s=1013}" |> parse_program |> evaluate_program
|> Printf.printf "final score: %d\n"