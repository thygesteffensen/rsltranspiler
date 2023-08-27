type TreeNode<'T> =
    | Node of value: 'T * children: list<TreeNode<'T>>

type FlattenedNode<'T> = 
    { Value: 'T; Depth: int }

let rec flattenTree depth (tree: TreeNode<'T>) =
    match tree with
    | Node(value, children) ->
        { Value = value; Depth = depth } :: (List.collect (flattenTree (depth + 1)) children)

let printFlattenedTree (flattened: list<FlattenedNode<'T>>) =
    flattened |> List.iter (fun node -> 
        printfn "%s%s" (String.replicate node.Depth "  ") node.Value)

let tree =
    Node("Root", [
        Node("Child1", [
            Node("Grandchild1", []);
            Node("Grandchild2", [])
        ]);
        Node("Child2", [
            Node("Grandchild3", [])
        ])
    ])

let flattened = flattenTree 0 tree
printFlattenedTree flattened
