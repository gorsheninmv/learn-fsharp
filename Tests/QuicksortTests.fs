module QuicksortTests

open System
open TailRecursion
open NUnit.Framework
open FsUnit

let generateRandomList n =
    let rnd = Random(int DateTime.Now.Ticks)
    List.init n (ignore >> rnd.Next)

let data = generateRandomList 10_000_000

let sorted list =
    list |> Seq.pairwise |> Seq.forall(fun (a, b) -> a <= b)

[<Test>]
let ``parallel quicksort should sort data`` () =
    data |> quicksortParallel |> should be ascending

[<Test>]
let ``cps quicksort should sort data`` () =
    data |> quicksort |> should be ascending

[<Test>]
[<Ignore("stackoverflow")>]
let ``plain quicksort should sort data`` () =
    data |> plainQuicksort |> should be ascending

(*
let generateRandomList n =
    let rnd = Random(int DateTime.Now.Ticks)
    List.init n (ignore >> rnd.Next)

let performSpan f =
    let sw = Stopwatch()
    sw.Start()
    f ()
    sw.Stop()
    printfn $"{sw.Elapsed}"

let sorted list =
    list |> Seq.pairwise |> Seq.forall(fun (a, b) -> a <= b)

let c = Console.ReadKey().KeyChar
let generateLongArray () =
    generateRandomList 1_000_000
let validate = sorted >> printfn "%b"

match c with
| '1' -> performSpan (fun () -> generateLongArray () |> quicksortParallel |> validate)
| '2' -> performSpan (fun () -> generateLongArray () |> plainQuicksort |> validate)
| _ -> printfn "invalid command"
*)
