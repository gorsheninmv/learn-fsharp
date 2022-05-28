namespace Pipeline

module Builder =

    let bind switchFn twoTrackInput =
        match twoTrackInput with
        | Ok success -> switchFn success
        | Error failure -> Error failure

    let map f result =
        match result with
        | Ok success -> Ok (f success)
        | Error failure -> Error failure

    let mapError f result =
        match result with
        | Ok success -> Ok success
        | Error failure -> Error (f failure)

module Implementation =

    open System

    type Apple = Apple
    type AppleError = AppleError of string

    type Banana = Banana
    type BananaError = BananaError of string

    type Cherry = Cherry
    type CherryError = CherryError of string

    type Lemon = Lemon
    type LemonError = LemonError of string

    type FunctuionA = Apple -> Result<Banana, AppleError>
    type FunctuionB = Banana -> Result<Cherry, BananaError>
    type FunctuionC = Cherry -> Result<Lemon, CherryError>

    type FruitError =
        | AppleErrorCase of AppleError
        | BananaErrorCase of BananaError
        | CherryErrorCase of CherryError
        | LemonErrorCase of LemonError

    let rnd () =
        let rnd = Random (int DateTime.Now.Ticks)
        fun () -> rnd.Next ()

    let canTransformWithRandom rnd () =
        let seed = rnd ()
        seed % 2 = 0

    let canTransform = canTransformWithRandom (() |> rnd)

    let functionA: FunctuionA = fun apple ->
        match canTransform () with
        | true -> Ok Banana
        | _ -> Error (AppleError "Non transformed apple")

    let functionB: FunctuionB = fun banana ->
        match canTransform () with
        | true -> Ok Cherry
        | _ -> Error (BananaError "Non transformed banana")

    let functionC: FunctuionC = fun cherry ->
        match canTransform () with
        | true -> Ok Lemon
        | _ -> Error (CherryError "Non transformed cherry")

    let pipeFruits = (functionA >> Result.mapError AppleErrorCase
        >> Result.bind (functionB >> Result.mapError BananaErrorCase)
        >> Result.bind (functionC >> Result.mapError CherryErrorCase))

module Tests =

    open NUnit.Framework
    open Implementation

    [<Test>]
    let ``run pipeline demo`` () =
        let apples = List.init 10 (fun _ -> Apple)
        apples |> List.map (fun apple -> pipeFruits apple) |> printfn "%A"
