namespace NumericAlgorithms

module Implementation =

  // gcd(a, b) = gcd(b, a % b)
  let rec gcd a b =
    let r = a % b
    match r with
    | 0 -> b
    | _ -> gcd b r


module Tests =

  open FsUnit
  open NUnit.Framework
  open Implementation

  [<Test>]
  let ``test gcd`` () =
    gcd 100 15 |> should equal 5
    gcd 15 100 |> should equal 5
