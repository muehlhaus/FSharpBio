namespace FSharpBio.ML.Tests

module DistanceMetrics =
    
    open FSharpBio.ML.Unsupervised.DistanceMetrics
    open NUnit.Framework
    open FsUnit    
    
    let vector1 = seq [1.;2.;3.;4.;5.;]

    [<Test>]
    let ``should return zero euclidian distance between the same vector`` () =
         euclidean vector1 vector1 |> should equal 0.
         