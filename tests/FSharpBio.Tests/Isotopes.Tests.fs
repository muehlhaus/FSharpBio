namespace FSharpBio.Tests


module String = 

    open FSharpBio.Isotopes
    open NUnit.Framework
    open FsUnit


    [<Test>]
    let ``should return that "true" can be converted to bool`` () =
         create "H" 1 1 1.00782503207 0.999885 1.007947
         |> should equal Table.H1
         


