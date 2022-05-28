module ProgressionTests

open TailRecursion
open NUnit.Framework
open FsUnit

let input = 1_000_000UL
let expectedOutput = 500_000_500_000UL

[<Test>]
let ``sumOf with accumulator should return valid sum`` () =
    input |> factorial |> should equal expectedOutput

[<Test>]
let ``sumOf with cps should return valid sum`` () =
    input |> sumOf |> should equal expectedOutput

[<Test>]
[<Ignore("stackoverflow")>]
let ``recursive sumOf should return valid sum`` () =
    input |> plainFactorial |> should equal expectedOutput
