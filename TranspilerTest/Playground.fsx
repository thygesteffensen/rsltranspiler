
let l1  = ["x1";"x2"]
let l2  = ["y1";"y2"]
let l3  = ["z1";"z2"]

List.foldBack (fun e a -> $"{e} | {a}") l3 ""

String.concat " | " ["t1"]