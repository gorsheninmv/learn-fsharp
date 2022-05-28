namespace Bst

module Implementation =

    type Tree<'a> =
        | Empty
        | Node of value: 'a * left: Tree<'a> * right: Tree<'a>

    let isBst tree =
        let rec isBstInternal tree lo hi =
            match tree with
            | Empty -> true
            | Node (value, left, right) ->
                match lo, hi with
                | Some lo, _ when value < lo -> false
                | _, Some hi when value > hi -> false
                | _ ->
                    let lo' = defaultArg lo value |> max value |> Some
                    let hi' = defaultArg hi value |> min value |> Some
                    isBstInternal left lo hi' &&
                    isBstInternal right lo' hi

        isBstInternal tree None None


module Tests =

    open NUnit.Framework
    open FsUnit
    open Implementation

    [<Test>]
    let ``empty tree is bst`` () =
        let tree = Empty
        tree |> isBst |> should equal true
