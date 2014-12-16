#if INTERACTIVE
    #r "D:/Development/FSharp/FSharpBio/bin/MathNet.Numerics.dll"
    #r "D:/Development/FSharp/FSharpBio/bin/MathNet.Numerics.Fsharp.dll"
    #r "D:/Development/FSharp/FSharpBio/bin/FSharp.CoreX.dll"
    #r "D:/Development/FSharp/FSharpBio/bin/FSharpBio.Statistics.dll"
    (*[omit:(other references omitted)]*)
    #r "D:/Development/FSharp/FSharpBio/packages/NUnit.2.6.3/lib/nunit.framework.dll"
    #r "D:/Development/FSharp/FsharpBio/packages/FsUnit.1.2.1.0/Lib/Net40/FsUnit.NUnit.dll"
    (*[/omit]*)
#else
namespace FSharpBio.Statistics.Tests

module StatisticalMeasureTest = 
#endif

    open MathNet.Numerics.LinearAlgebra.Double
    open MathNet.Numerics.LinearAlgebra
    open FSharp.CoreX
    open FSharpBio.Statistics
    open NUnit.Framework
    open FsUnit

    let inputA = [[90.; 60.; 90.;];[90.; 90.; 30.; ];[60.; 60.; 60.; ];[60.; 60.; 90.; ];[30.; 30.; 30.; ];] |> DenseMatrix.ofRowList
    let covA   = [[504.; 360.; 180.; ];[360.; 360.; 0.; ];[180.; 0.; 720.; ];] |> DenseMatrix.ofRowList
    

    [<Test>]
    let ``should return the cov matrix`` () =
            Matrix.cov inputA |> should equal covA
