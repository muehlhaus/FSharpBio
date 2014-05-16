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

module StatisticalMeasure = 
#endif

    open FSharpBio.Statistics.Descriptive.StatisticalMeasure
    open NUnit.Framework
    open FsUnit

    //http://tomasp.net/blog/2013/great-open-source/
    //might be missing:
    // --> #r "../../../packages/FsCheck.0.9.1.0/lib/net40-Client/FsCheck.dll"
    // -->  #load "../../Common/FsUnit.fs"(*[/omit]*)    
    let testData = [1.;2.;3.;4.;5.;6.;]

    [<Test>]
    let ``should return the mean`` () =
            mean testData |> should equal 3.5
