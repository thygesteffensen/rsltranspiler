
open System

let l1  = ["x1";"x2"]
let l2  = ["y1";"y2"]
let l3  = ["z1";"z2"]

List.foldBack (fun e a -> $"{e} | {a}") l3 ""

String.concat " | " ["t1"]


let inputs = [1;]

inputs |> List.reduce (fun a b -> a * 10 + b)



let mutable m: Map<int * string, string> = Map.empty

m <- m.Add((2, "QR1"), "")
m <- m.Add((1, "TR1"), "")
m <- m.Add((3, "AR1"), "")
m <- m.Add((9, "BR1"), "")
m <- m.Add((5, "RR1"), "")
m <- m.Add((6, "MR1"), "")


Map.foldBack (fun k v a -> printf $"{k}\n") m ()


