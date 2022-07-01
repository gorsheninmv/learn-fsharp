namespace Tests.Common

[<AutoOpen>]
module Helpers =

  let getInnerValueByIndex<'t, 's> (index: int) (a: 's) =
    let _, x = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(a, typeof<'s>)
    x.GetValue(index) :?> 't

  let getInnerValue<'t, 's> (a: 's) = getInnerValueByIndex<'t, 's> 0 a
