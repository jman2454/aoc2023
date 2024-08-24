let hash s = 
  Seq.fold_left (fun acc c -> (17 * (acc + (Char.code c))) mod 256) 0 s

let instructions s = String.split_on_char ',' s
let hide_newlines s = String.to_seq s |> Seq.filter ((<>) '\n')

let ans s = instructions s |> List.map hide_newlines |> List.map hash |> List.fold_left (+) 0

let () = ans "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" |> Printf.printf "ans: %d\n"