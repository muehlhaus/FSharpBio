namespace FSharpBio.IO.Tests

module FileIO =

    open FSharpBio.IO.FileIO
    open NUnit.Framework
    open FsUnit


    [<Test>]
    let ``should return that "true" can be converted to bool`` () =
         cleanFileName "test/.fsx" |> should equal "test.fsx"
