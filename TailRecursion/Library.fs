namespace TailRecursion

[<AutoOpen>]
module QuickSort =

    open System
    open System.Threading.Tasks

    let quicksortParallel xs =
        let rec quicksortParallelInternal depth xs =
            match xs with
            | [] -> []
            | first :: rest ->
                let lhs, rhs = List.partition (fun n -> n < first) rest
                if depth < 0 then
                    let left = quicksortParallelInternal depth lhs
                    let right = quicksortParallelInternal depth rhs
                    left @ (first :: right)
                else
                    let left = Task.Run(fun () -> quicksortParallelInternal (depth-1) lhs)
                    let right = Task.Run(fun () -> quicksortParallelInternal (depth-1) rhs)
                    left.Result @ (first :: right.Result)

        let procsCount = float Environment.ProcessorCount
        let depth = int (Math.Log2 procsCount) + 4
        quicksortParallelInternal depth xs

    let quicksort xs =
        let rec quicksortInternal xs cont =
            match xs with
            | [] -> cont []
            | first :: rest ->
                let lhs, rhs = List.partition (fun n -> n < first) rest
                quicksortInternal lhs (fun left ->
                    quicksortInternal rhs (fun right -> cont (left @ (first :: right))))

        quicksortInternal xs id

    let rec plainQuicksort xs =
        match xs with
        | [] -> []
        | first :: rest ->
            let lhs, rhs = List.partition (fun n -> n < first) rest
            let left = plainQuicksort lhs
            let right = plainQuicksort rhs
            left @ (first :: right)

[<AutoOpen>]
module Factorial =

    let rec plainFactorial n: uint64 =
        match n with
        | 0UL -> 0UL
        | _ -> n + plainFactorial (n-1UL)

    let factorial n: uint64 =
        let rec factorialInternal (n: uint64) (acc: uint64) =
            match n with
            | 0UL -> acc
            | _ -> factorialInternal (n-1UL) (n + acc)

        factorialInternal n 0UL

    let sumOf n =
        let rec sumOfInternal (n: uint64) cont =
            match n with
            | 0UL -> cont 0UL
            | _ -> sumOfInternal (n-1UL) (fun x -> cont (x + n))

        sumOfInternal n id

    let sumOf2 n =
        let rec sumOfInternal n cont =
            if n < 1 then
                cont ()
            else
                sumOfInternal (n-1) (fun () -> n + cont ())

        sumOfInternal n (fun () -> 0)
