namespace Bst

module Implementation =

  open Common

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

  let height node =
    match node with
    | Empty -> -1
    | Node node -> node.height

  let nodeHeightDiff node =
    (height node.left) - (height node.right)

  let treeHeightDiff tree =
    match tree with
    | Empty -> 0
    | Node node -> nodeHeightDiff node

  let ll node =
    let y =
      match node.left with
      | Node node -> node
      | Empty -> raise InvalidCaseException
    let a = y.left
    let b = y.right
    let c = node.right
    let x = Node { node with left = b; right = c; height = 1 + max (height b) (height c) }
    { y with left = a; right = x; height = 1 + max (height a) (height x) }

  let rr node =
    let y =
      match node.right with
      | Node node -> node
      | Empty -> raise InvalidCaseException
    let a = node.left
    let b = y.left
    let c = y.right
    let x = Node { node with left = a; right = b; height = 1 + max (height a) (height b) }
    { y with left = x; right = c; height = 1 + max (height x) (height c) }

  let rl node =
    let y =
      match node.right with
      | Node node -> node
      | Empty -> raise InvalidCaseException
    let y = ll y
    let node = { node with right = Node y }
    rr node

  let lr node: Node<'a> =
    let y =
      match node.left with
      | Node node -> node
      | Empty -> raise InvalidCaseException
    let y = rr y
    let node = { node with left = Node y }
    ll node

  let balance node =
    let diff = nodeHeightDiff node
    match diff with
    | -2 when (treeHeightDiff node.right) = -1 -> rr node
    | -2 when (treeHeightDiff node.right) = 1 -> rl node
    | -2 -> raise InvalidCaseException
    | 2 when (treeHeightDiff node.left) = 1 -> ll node
    | 2 when (treeHeightDiff node.left) = -1 -> lr node
    | 2 -> raise InvalidCaseException
    | diff when (abs diff) > 2 -> raise InvalidCaseException
    | _ -> node

  let insert value tree =

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
          | _ -> raise (InvalidCaseExceptionWithMessage $"unexpected value = {value}")

    Node (insertInternal tree value)

  let rec find value tree =
   match tree with
   | Empty -> Empty
   | Node node ->
     match node with
     | node when value < node.value -> find value node.left
     | node when value > node.value -> find value node.right
     | _ -> Node node

  let rec remove value tree =

   let rec findMin node =
     match node.left with
     | Empty -> node
     | Node node -> findMin node

   // returns balanced subtree
   let removeThis node =
     match node.left, node.right with
     | Node left, Empty -> Node left
     | Empty, Node right -> Node right
     | Empty, Empty -> Empty
     | Node _, Node right ->
       let nodeToReplace = findMin right
       let rightAfterRemove = remove nodeToReplace.value (Node right)
       Node (balance { node with value = nodeToReplace.value; right = rightAfterRemove })

   match tree with
   | Empty -> Empty
   | Node node ->
     match node with
     | node when value < node.value ->
       let left = remove value node.left
       Node (balance { node with left = left })
     | node when value > node.value ->
       let right = remove value node.right
       Node (balance { node with right = right })
     | node -> removeThis node

module Tests =

  open NUnit.Framework
  open FsUnit
  open Common
  open Tests.Common
  open Implementation

  module SimpleTreeBuilder =

    let build values =
      let rec insert value tree =
        match tree with
        | Empty -> Node { value = value; left = Empty; right = Empty; height = 0 }
        | Node node ->
            match node with
            | node when value < node.value ->
              let left = insert value node.left
              Node { node with left = left }
            | node when value > node.value ->
              let right = insert value node.right
              Node { node with right = right }
            | _ -> raise (InvalidCaseExceptionWithMessage $"unexpected value = {value}")

      let fillTree values =
        Array.fold (fun tree value -> insert value tree) Empty values

      let rec calcHeights node =
        match node.left, node.right with
        | Empty, Empty -> node
        | Node _, Empty | Empty, Node _ -> { node with height = 1 }
        | Node left, Node right ->
          let left = calcHeights left
          let right = calcHeights right
          let height = 1 + max left.height right.height
          { node with left = Node left; right = Node right; height = height }

      let tree = fillTree values
      if isBst tree then
        match tree with
        | Empty -> failwith "unable build empty tree"
        | Node node -> Node (calcHeights node)
      else
        failwith "the tree is not bst"

  let leaf = {
    value = 0
    left = Empty
    right = Empty
    height = 0
  }

  let isSameTrees first second =
    LanguagePrimitives.PhysicalEquality first second

  let fillTree values =
    Array.fold (fun tree value -> insert value tree) Empty values

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

  [<TestCase([| 42; 10; 60; 50; 70 |], 2)>]
  [<TestCase([| 42; 60 |], 1)>]
  let ``bst height should be valid`` values treeHeight =
    let tree = fillTree values
    match tree with
    | Empty -> raise InvalidCaseException
    | Node node -> node.height |> should equal treeHeight

  [<Test>]
  let ``insert yields new tree`` () =
    let tree = fillTree [| 42; 10; 60; 50 |]
    let updatedTree = tree |> insert 70
    isSameTrees tree updatedTree |> should equal false

  [<Test>]
  let ``insert yields the same sub tree`` () =
    let tree = fillTree [| 42; 10; 60; 50 |]
    let updatedTree = tree |> insert 70
    match tree, updatedTree with
    | Node node, Node updatedNode -> isSameTrees node.left updatedNode.left |> should equal true
    | _ -> raise InvalidCaseException

  [<Test>]
  let ``insert existed value throws error`` () =
    let tree = Node { leaf with value = 42 }
    (fun () -> insert 42 tree |> ignore) |> should throw typeof<InvalidCaseExceptionWithMessage>

  [<Test>]
  let ``ll check`` () =
    let tree = fillTree [| 10; 5; 2 |]
    let expectedTree = Node {
      value = 5
      left = Node { leaf with value = 2 }
      right = Node {leaf with value = 10}
      height = 1
    }
    tree |> should equal expectedTree

  [<Test>]
  let ``rr check`` () =
    let tree = fillTree [| 10; 15; 20 |]
    let expectedTree = Node {
      value = 15
      left = Node { leaf with value = 10 }
      right = Node {leaf with value = 20}
      height = 1
    }
    tree |> should equal expectedTree

  [<Test>]
  let ``rl check`` () =
    let tree = fillTree [| 10; 5; 16; 20; 15; 14 |]
    let expectedTree = Node {
      value = 15
      left = Node {
        value = 10
        left = Node { leaf with value = 5 }
        right = Node { leaf with value = 14 }
        height = 1
      }
      right = Node { leaf with value = 16; right = Node { leaf with value = 20 }; height = 1 }
      height = 2
    }
    tree |> should equal expectedTree

  [<Test>]
  let ``lr check`` () =
    let tree = fillTree [| 10; 16; 5; 6; 4; 7 |]
    let expectedTree = Node {
      value = 6
      left = Node { leaf with value = 5; left = Node { leaf with value = 4 }; height = 1 }
      right = Node {
        value = 10
        left = Node { leaf with value = 7 }
        right = Node { leaf with value = 16 }
        height = 1
      }
      height = 2
    }
    tree |> should equal expectedTree

  [<Test>]
  let ``node found if value exists`` () =
    let tree = fillTree [| 10; 5; 15; 4; 6; 14; 16 |]
    let tree = find 4 tree
    Assert.Multiple (fun () ->
      tree |> should be (ofCase<@ Tree<int>.Node @>)
      tree |> getInnerValue<Node<int>> |> fun node -> node.value |> should equal 4)

  [<Test>]
  let ``node not found if value does not exist`` () =
    let tree = fillTree [| 10; 5; 15; 4; 6; 14; 16 |]
    let tree = find 42 tree
    tree |> should be (ofCase<@ Tree<int>.Empty @>)

  [<Test>]
  let ``remove root works as expected`` () =
    let tree = fillTree [| 15; 10; 20; 5; 14; 25 |]
    let tree = remove 15 tree
    let expectedTree = SimpleTreeBuilder.build [| 20; 10; 25; 5; 14 |]
    tree |> should equal expectedTree

  [<Test>]
  let ``how immutability works`` () =
    let tree = fillTree [| 42; 10; 60; 50 |]
    let copy = tree
    isSameTrees tree copy |> should equal true
