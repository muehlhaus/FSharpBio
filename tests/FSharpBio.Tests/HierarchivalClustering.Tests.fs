#if INTERACTIVE
    #light
    #r "D:/Development/FSharp/FSharpBio/bin/FSharp.CoreX.dll"
    #r "D:/Development/FSharp/FSharpBio/bin/FSharpBio.ML.dll"
    (*[omit:(other references omitted)]*)
    #r "D:/Development/FSharp/FSharpBio/packages/NUnit.2.6.3/lib/nunit.framework.dll"
    #r "D:/Development/FSharp/FsharpBio/packages/FsUnit.1.2.1.0/Lib/Net40/FsUnit.NUnit.dll"
(*[/omit]*)
#else
namespace FSharpBio.ML.Tests

module HierarchivalClustering = 
#endif


    open FSharpBio.ML.Unsupervised.HierarchivalClustering

    // Binary tree example
    let binaryExample = 
        let c15 = createClusterValue 15 15
        let c14 = createClusterValue 14 14
        let c13 = createClusterValue 13 13
        let c12 = createClusterValue 12 12
        let c11 = createClusterValue 11 11
        let c10 = createClusterValue 10 10
        let c09 = createClusterValue  9  9
        let c08 = createClusterValue  8  8

        let c07 = createCluster 7 7.0 c14 c15
        let c06 = createCluster 6 6.0 c12 c13
        let c05 = createCluster 5 5.0 c10 c11
        let c04 = createCluster 4 4.0 c08 c09
        let c03 = createCluster 3 3.0 c06 c07
        let c02 = createCluster 2 2.0 c04 c05
        let c01 = createCluster 1 1.0 c02 c03
        c01


