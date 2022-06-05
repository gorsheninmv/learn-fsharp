namespace Bst

module Implementation =

  type Node<'a> = {
    value: 'a
    left: Tree<'a>
    right: Tree<'a>
    height: int
  }
  and Tree<'a> =
    | Empty
    | Node of Node<'a>

  let isBst tree =
    let rec isBstInternal tree lo hi =
      match tree with
      | Empty -> true
      | Node { value = value; left = left; right = right } ->
        match lo, hi with
        | Some lo, _ when value < lo -> false
        | _, Some hi when value > hi -> false
        | _ ->
          let lo' = defaultArg lo value |> max value |> Some
          let hi' = defaultArg hi value |> min value |> Some
          isBstInternal left lo hi' &&
          isBstInternal right lo' hi

    isBstInternal tree None None

  let insert value tree =

    let height node =
      match node with
      | Empty -> -1
      | Node node -> node.height

    let balance node =
      let lh = height node.left
      let rh = height node.right
      let diff = abs (lh - rh)
      match diff with
      | diff when diff < 2 -> node
      | _ -> failwith "balance required"

    let rec insertInternal tree value =
      match tree with
      | Empty -> { value = value; left = Empty; right = Empty; height = 0 }
      | Node node ->
          match node with
          | node when value < node.value ->
            let left = Node (insertInternal node.left value)
            balance { node with left = left; height = 1 + max (height left) (height node.right) }
          | node when value > node.value ->
            let right = Node (insertInternal node.right value)
            balance { node with right = right; height = 1 + max (height right) (height node.right) }
          | _ -> failwith $"unexpected value of value = {value}"

    Node (insertInternal tree value)

open Implementation

module Tests =

  open NUnit.Framework
  open FsUnit
  open Implementation

  let leaf = {
    value = 0
    left = Empty
    right = Empty
    height = 0
  }

  let isSameTrees first second =
    LanguagePrimitives.PhysicalEquality first second

  [<Test>]
  let ``empty tree is bst`` () =
    let tree = Empty
    tree |> isBst |> should equal true

  [<Test>]
  let ``one node tree is bst`` () =
    let tree = Node { leaf with value = 1; }
    tree |> isBst |> should equal true

  [<Test>]
  let ``isBst returns false when tree is not bst`` () =
    let tree = Node { leaf with
      value = 20
      left = Node { leaf with value = 10 }
      right = Node { leaf with
        value = 30
        left = Node { leaf with value = 5 }
        right = Node { leaf with value = 40 }
      }
    }
    tree |> isBst |> should equal false

  [<Test>]
  let ``isBst returns true when tree is bst`` () =
    let tree = Node { leaf with
      value = 20
      left = Node { leaf with value = 10 }
      right = Node { leaf with
        value=30
        left = Node { leaf with value = 25 }
        right = Node { leaf with value = 40 }
      }
    }
    tree |> isBst |> should equal true

  [<Test>]
  let ``bst height should be valid`` () =
    let tree = Empty |> insert 42 |> insert 10 |> insert 60 |> insert 50 |> insert 70
    match tree with
    | Empty -> failwith "tree should be node"
    | Node node -> node.height |> should equal 2

  [<Test>]
  let ``bst height should be valid 2`` () =
    let tree = Empty |> insert 42 |> insert 60
    match tree with
    | Empty -> failwith "tree should be node"
    | Node node -> node.height |> should equal 1

  [<Test>]
  let ``insert yields new tree`` () =
    let tree = Empty |> insert 42 |> insert 10 |> insert 60 |> insert 50
    let updatedTree = tree |> insert 70
    isSameTrees tree updatedTree |> should equal false

  [<Test>]
  let ``insert yields the same sub tree`` () =
    let tree = Empty |> insert 42 |> insert 10 |> insert 60 |> insert 50
    let updatedTree = tree |> insert 70
    match tree, updatedTree with
    | Node node, Node updatedNode -> isSameTrees node.left updatedNode.left |> should equal true
    | _ -> failwith "both trees should be nodes"

  [<Test>]
  let ``insert existed value throws error`` () =
    let tree = Node { leaf with value = 42 }
    (fun () -> insert 42 tree |> ignore) |> should throw typeof<System.Exception>

  let a = [|42; 10|]

  [<TestCase([| 42; 10; 60; 50; 70; 90 |])>]
  [<TestCase([| 42; 60; 70 |])>]
  let ``if diff more than 2 balance required`` valuesToInsert =
    let fillTree = Array.fold (fun tree value -> insert value tree) Empty
    (fun () -> fillTree valuesToInsert |> ignore) |> should (throwWithMessage "balance required") typeof<System.Exception>
