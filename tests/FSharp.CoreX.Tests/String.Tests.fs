namespace FSharp.CoreX.Tests

module String = 

    open FSharp.CoreX.String
    open NUnit.Framework
    open FsUnit


    [<Test>]
    let ``should return that "true" can be converted to bool`` () =         
         isBool "true" |> should equal true
         