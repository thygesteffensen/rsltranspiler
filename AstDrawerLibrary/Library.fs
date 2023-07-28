namespace AstDrawerLibrary
(* 
FROM PROJECT 1
Library.fsx
Authors:
Adam Ømosegård Bischoff     s174295
Ádám Kovács                 s210728
Thyge Skødt Steffensen      s175176

Date: 08.06.2021

*)

(* FROM DRAWING TREES *)

module Library =

    type Tree<'a> = Node of 'a * ('a Tree list)
    type Extent = (float * float) list

    let moveTree (Node((label, x), subtrees), (x': float)) = Node((label, (x + x')), subtrees)

    let moveExtent (e: Extent, x) =
        List.map (fun (p, q) -> (p + x, q + x)) e

    let rec merge (e1: Extent) (e2: Extent) =
        match (e1, e2) with
        | ([], qs) -> qs
        | (ps, []) -> ps
        | ((p, _) :: ps, (_, q) :: qs) -> (p, q) :: merge ps qs

    let mergeList es = List.fold merge [] es

    let rec fit p q =
        match p, q with
        | ((_, p') :: ps), ((q', _) :: qs) -> max (fit ps qs) (p' - q' + 1.0)
        | _, _ -> 0.0

    let fitlistl es =
        let rec fitlistl' acc es =
            match es with
            | [] -> []
            | e :: es ->
                let x = fit acc e
                x :: fitlistl' (merge acc (moveExtent (e, x))) es

        fitlistl' [] es

    let fitlistr es =
        let rec fitlistr' acc es =
            match es with
            | [] -> []
            | e :: es ->
                let x = -(fit e acc)
                x :: fitlistr' (merge (moveExtent (e, x)) acc) es

        List.rev (fitlistr' [] (List.rev es))

    let mean (x, y) = (x + y) / 2.0

    let fitlist es =
        List.map mean (List.zip (fitlistl es) (fitlistr es))

    let rec design' (Node(label, subtrees)) =
        let (tress, extents) = List.unzip (List.map design' subtrees)
        let positions = fitlist extents

        let pTrees = List.map moveTree (List.zip tress positions)

        let pExtents = List.map moveExtent (List.zip extents positions)

        let resultExtent = (0.0, 0.0) :: mergeList pExtents
        let resultTree = Node((label, 0.0), pTrees)

        (resultTree, resultExtent)

    let design tree = fst (design' tree)


    (* HELPER FUNCTIONS *)

    let concat (s1: string) (s2: string) = System.String.Concat([| s1; s2 |])

    let plus (s1: string) (s2: string) = s1 + s2

    let rec longestPath t c =
        match t with
        | Node(_, []) -> c + 1
        | Node(_, children) -> List.max (List.map (fun e -> longestPath e (c + 1)) children)

    let rec longestLabel (Node((label, _), children)) =
        match children with
        | [] -> String.length (string label)
        | _ -> max (List.max (List.map (fun e -> longestLabel e) children)) (String.length (string label))

    let rec getWidths (Node((_, pos), children)) =
        match children with
        | [] -> (pos, pos)
        | _ ->
            List.fold
                (fun (max', min') (maxElem, minElem) -> ((max max' (maxElem + pos)), (min min' (minElem + pos))))
                (pos, pos)
                (List.map getWidths children)


    let writeToFile filename string =
        System.IO.File.WriteAllText(filename, string)

    (* REQUIRED FUNCTIONS *)

    let toPSfast t =
        let fontsize = 12
        let charHeight = 8.0
        let charWidth = 7.0
        let nodeSpacing = 3.2
        let yDistance = 30.0
        let labelSpacing = 2.0

        let sb1 = System.Text.StringBuilder()

        let xScale = -15.0 + (nodeSpacing * float (longestLabel t))

        let (maxPos, minPos) = getWidths t

        let startX = ((abs minPos) * xScale + (xScale / 2.0))

        let startY = (labelSpacing + yDistance + charHeight) * float (longestPath t 0)

        sb1
            .Append("%!PS\n/DejaVu ")
            .Append(string (fontsize))
            .Append(" selectfont\n<< /PageSize [")
            .Append(string ((maxPos + abs minPos) * xScale + xScale))
            .Append(" ")
            .Append(string (startY + 20.0))
            .Append("] >> setpagedevice\n")
            .Append(string (startX - nodeSpacing / 2.0))
            .Append(" ")
            .Append(string (startY))
            .Append(" moveto\n")
        |> ignore

        let rec drawNode (Node((label, pos), children)) (x, y) (sb: System.Text.StringBuilder) =
            let x' = x + (pos * xScale)
            let y' = y - float charHeight

            sb
                .Append(string (x' - (float (String.length (string label)) * charWidth / 2.0)))
                .Append(" ")
                .Append(string y')
                .Append(" moveto\n")
                .Append("(")
                .Append(string label)
                .Append(") show\n")
            |> ignore

            drawLine (x', y' - labelSpacing) sb children

        and drawLine (x, y) (sb: System.Text.StringBuilder) =
            function
            | [] -> ()
            | (Node((_, pos), _)) as n :: ns ->
                let x' = x + (pos * xScale)
                let y' = y - yDistance

                sb
                    .Append(string x)
                    .Append(" ")
                    .Append(string y)
                    .Append(" moveto\n")
                    .Append(string x')
                    .Append(" ")
                    .Append(string y')
                    .Append(" lineto\n")
                |> ignore

                drawNode n (x, y' - labelSpacing) sb
                drawLine (x, y) sb ns

        drawNode t (startX, startY) sb1

        sb1.Append("stroke\nshowpage") |> ignore
        sb1.ToString()

    let posTreeToFile filename t = toPSfast t |> writeToFile filename

    let treeToFile filename tree =
        (design tree |> toPSfast) |> writeToFile filename
