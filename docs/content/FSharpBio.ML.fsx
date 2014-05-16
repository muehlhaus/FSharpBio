(*** hide ***)
#I "../../bin"


//#r @"D:\Development\FSharp\FSharpBio\bin\..
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.Fsharp.dll"
#r "FSharp.CoreX.dll"
#r "FSharpBio.dll"
#r "FSharp.Charting.dll"

open FSharp.CoreX
open FSharpBio.IO
open FSharpBio.IO.SchemaReader
open FSharpBio.IO.SchemaReader.Csv
open FSharpBio.IO.SchemaReader.Attribute
open MathNet.Numerics.LinearAlgebra.Generic
open MathNet.Numerics.LinearAlgebra.Double
open FSharpBio.ML.Unsupervised


(**
FSharpBio
=========

*)


// ##################################################################
// Examples: Csv-reader reads iris data set
type irisItem = 
    { [<FieldAttribute("Sepal length")>] SepalLength : float
      [<FieldAttribute("Sepal width")>] SepalWidth : float
      [<FieldAttribute("Petal length")>] PetalLength : float
      [<FieldAttribute("Petal width")>] PetalWidth : float
      [<FieldAttribute("Species")>] Species : string }

let _ = FileIO.setWorkingDirectory __SOURCE_DIRECTORY__
let reader = new CsvReader<irisItem>()
let irisData = reader.ReadFile("./data/irisData.csv", ',', true) |> Seq.toList
let firstItem = 
    irisData |> Seq.averageBy (fun (item : irisItem) -> item.PetalLength)
let irisFeatures = 
    irisData 
    |> List.map 
           (fun ii -> 
           [ ii.SepalLength; ii.SepalWidth; ii.PetalLength; ii.PetalWidth ])
let irisLables = irisData |> List.map (fun ii -> ii.Species)
let irisFeaturesMatrix = DenseMatrix.ofList irisFeatures
// ---------------
// Principal component analysis
let adjCenter = PCA.toAdjustCenter irisFeaturesMatrix
let irisPCA = PCA.compute adjCenter irisFeaturesMatrix
let irisDataPCA = PCA.transform adjCenter irisPCA irisFeaturesMatrix
let irisrev = PCA.revert adjCenter irisPCA irisDataPCA

// Score Plot
open FSharp.Charting

// Plot loadings colored grouped by grouping function
let plotLoadingsColoredByGrouping (pcaComponents : PCA.Component []) 
    (labels : seq<string>) (grouping : string -> string) pcIndex1 pcIndex2 = 
    let pComponent1 = pcaComponents.[pcIndex1 - 1]
    let pComponent2 = pcaComponents.[pcIndex2 - 1]
    (Seq.zip3 (pComponent1.EigenVector) (pComponent2.EigenVector) labels)
    |> Seq.groupBy (fun (x, y, label) -> grouping label)
    |> Seq.map 
           (fun (key, values) -> 
           let nVal = values |> Seq.map (fun (x, y, l) -> (x, y))
           let nLab = values |> Seq.map (fun (x, y, l) -> l)
           Chart.Point(nVal, Name = key, Labels = nLab) 
           |> Chart.WithMarkers(Size = 15))
    |> Chart.Combine
    |> Chart.WithTitle
           (sprintf "PC %i (%.2f) versus PC %i (%.2f)" pComponent1.Index 
                (pComponent1.Proportion * 100.) pComponent2.Index 
                (pComponent2.Proportion * 100.))
    |> Chart.ShowChart

// Plot loadings colored grouped by grouping function
let plotScoresColoredByGrouping (transformedData : Matrix<float>) 
    (labels : seq<string>) (grouping : string -> string) pcIndex1 pcIndex2 = 
    (Seq.zip3 (transformedData.Column(pcIndex1 - 1)) 
         (transformedData.Column(pcIndex2 - 1)) labels)
    |> Seq.groupBy (fun (x, y, label) -> grouping label)
    |> Seq.map 
           (fun (key, values) -> 
           let nVal = values |> Seq.map (fun (x, y, l) -> (x, y))
           let nLab = values |> Seq.map (fun (x, y, l) -> l)
           Chart.Point(nVal, Name = key, Labels = nLab) 
           |> Chart.WithMarkers(Size = 15))
    |> Chart.Combine    
    |> Chart.ShowChart

plotLoadingsColoredByGrouping irisPCA 
    [ "Sepal length"; "Sepal width"; "Petal length"; "Petal width" ] 
    (fun x -> x) 1 2
plotScoresColoredByGrouping irisDataPCA irisLables (fun x -> x) 1 2

// ---------------
// Scattern matrix plot
let plotScatternMatrixColoredByGrouping (inputMatrix : Matrix<float>) 
    (labels : seq<string>) (grouping : string -> string) = 
    inputMatrix.ColumnEnumerator()
    |> Seq.collect (fun (i, outerColumn) -> 
           inputMatrix.ColumnEnumerator()
           |> Seq.map (fun (ii, innerColumn) -> 
                  (Seq.zip3 outerColumn innerColumn labels)
                  |> Seq.groupBy (fun (x, y, label) -> grouping label)
                  |> Seq.map 
                         (fun (key, values) -> 
                         let nVal = values |> Seq.map (fun (x, y, l) -> (x, y))
                         Chart.Point
                             (nVal, 
                              Name = (sprintf "%s%i:%i" key (i + 1) (ii + 1))))
                  |> Chart.Combine
                  |> Chart.WithTitle((sprintf "%i : %i" (i + 1) (ii + 1)))
                  |> Chart.WithMarkers(Style = ChartTypes.MarkerStyle.Circle))
           |> Seq.toList
           |> List.rev)
    |> Chart.RowsWithBreak inputMatrix.ColumnCount
    |> Chart.ShowChart

let plotScatternMatrix (inputMatrix : Matrix<float>) = 
    inputMatrix.ColumnEnumerator()
    |> Seq.collect (fun (i, outerColumn) -> 
           inputMatrix.ColumnEnumerator()
           |> Seq.map 
                  (fun (ii, innerColumn) -> 
                  Chart.Point(Seq.zip outerColumn innerColumn)
                  |> Chart.WithTitle((sprintf "%i : %i" (i + 1) (ii + 1)))
                  |> Chart.WithMarkers
                         (Color = System.Drawing.Color.DarkGray, 
                          Style = ChartTypes.MarkerStyle.Circle))
           |> Seq.toList
           |> List.rev)
    |> Chart.RowsWithBreak inputMatrix.ColumnCount
    |> Chart.ShowChart

plotScatternMatrix irisFeaturesMatrix
plotScatternMatrixColoredByGrouping irisFeaturesMatrix irisLables (fun x -> x)

// ---------------
// Kmeans clustering
// For random cluster inititalization use randomInitFactory:
let rng = new System.Random()
let randomInitFactory : IterativeClustering.CentroidsFactory<float []> = 
    IterativeClustering.randomCentroids<float []> rng
let cvmaxFactory : IterativeClustering.CentroidsFactory<float []> = 
    IterativeClustering.intitCVMAX
let kmeansResult = 
    IterativeClustering.kmeans <| DistanceMetrics.euclidean <| cvmaxFactory 
    <| (Matrix.toJaggedArrayRowWise irisFeaturesMatrix) <| 3

let chartsOfClassifiedData = 
    Matrix.toJaggedArrayRowWise irisFeaturesMatrix
    |> Seq.groupBy (fun dataPoint -> fst (kmeansResult.Classifier dataPoint))
    |> Seq.sortBy fst
    |> Seq.map (fun (key, values) -> 
           values
           |> Seq.map 
                  (fun v -> 
                  Chart.Line v 
                  |> Chart.WithStyling
                         (Color = System.Drawing.Color.Silver, BorderWidth = 1))
           |> Chart.Combine
           |> Chart.WithTitle(key.ToString()))
    |> Chart.Rows
    |> Chart.ShowChart

// ---------------
// Hieracical clustering
let clusterTree = 
    HierarchivalClustering.generate<float list> DistanceMetrics.euclidean 
        HierarchivalClustering.Linker.weightedGroupAverageLwLinker irisFeatures
let get = HierarchivalClustering.getClusterMemberLabels clusterTree

let tmp = 
    get
    |> List.map (fun l -> 
           ((List.nth l 2), 
            (l
             |> List.rev
             |> List.head)))
    |> List.sortBy snd

let test = Seq.zip (tmp |> Seq.map fst) irisLables

test |> Seq.countBy (fun x -> x)
HierarchivalClustering.printHClust clusterTree 
|> Seq.write "D:/mySite/test2.json"



open FSharpBio.Statistics.Descriptive.StatisticalMeasure

let words = 
    [ "bag"; "across"; "on"; "insane"; "by"; "monastery"; "relief"; "slope"; 
      "scoundrel"; "with"; "neither"; "pretentious"; "solid"; "this"; "for"; 
      "therefore"; "generality"; "arise"; "blot"; "infectious" ]
let wordLength = 
    [ 3.; 6.; 2.; 6.; 2.; 9.; 6.; 5.; 9.; 4.; 7.; 11.; 5.; 4.; 3.; 9.; 10.; 5.; 
      4.; 10. ]
let numberOfLines = 
    [ 14.; 7.; 11.; 9.; 9.; 4.; 8.; 11.; 5.; 8.; 2.; 4.; 12.; 9.; 8.; 1.; 4.; 
      13.; 15.; 6. ]
let wordFrequency = 
    [ 8.; 230.; 700.; 1.; 500.; 1.; 9.; 2.; 1.; 700.; 7.; 1.; 4.; 500.; 900.; 3.; 
      1.; 10.; 1.; 1. ]
let wordEntries = 
    [ 6.; 3.; 12.; 2.; 7.; 1.; 1.; 6.; 1.; 5.; 2.; 1.; 5.; 9.; 7.; 1.; 1.; 4.; 
      4.; 2.; ]

let wordDataSet = 
    DenseMatrix.ofColumnsList (words.Length) 2 [ wordLength; numberOfLines ]
 //wordFrequency;wordEntries]
let adjCenterWordPCA = PCA.toAdjustCenter wordDataSet
let wordPCA = PCA.compute adjCenterWordPCA wordDataSet
let V = PCA.getFeatureMatrixOfComponents wordPCA
let variables = adjCenterWordPCA PCA.AdjustmentDirection.Obverse wordDataSet
let F = PCA.transform adjCenterWordPCA wordPCA wordDataSet

let sd = 
    wordPCA
    |> Array.map (fun c -> sqrt c.EigenValue)
    |> DenseVector.OfEnumerable

F.Transpose() * sd.ToColumnMatrix()

let M = 
    DenseMatrix.OfArray([| [| -1.; 1. |]
                           [| -1.; 1. |] |]
                        |> Array2D.ofJaggedArray)

F.PointwiseMultiply(M)

let factor = 5.0

let f (comps : PCA.Component []) = 
    comps |> Array.map (fun c -> 
                 let evar = 
                     c.EigenVector 
                     |> FSharpBio.Statistics.Descriptive.StatisticalMeasure.var
                 evar)

//c.EigenVector |> Array.map (fun i -> sqrt (((i * i) * evar) / c.EigenValue))
let tmp2 = f wordPCA
let a = [| -0.536875; 0.843661 |]

sqrt 
    (a.[1] * a.[1] 
     * (FSharpBio.Statistics.Descriptive.StatisticalMeasure.var (F.Column(0))) 
     / 392.)

let av = variables.Column(0) //DenseVector.OfEnumerable([2.;-3.;4.;])//F.Column(0)
let bv = F.Column(0) //DenseVector.OfEnumerable([5.;2.;1.;])//variables.Column(0)
let cosV = (av * bv) / (av.Norm(2.) * bv.Norm(2.))

FSharpBio.Statistics.Descriptive.Correlation.pearsonCorrelation av bv

let v = FSharpBio.Statistics.Descriptive.StatisticalMeasure.var (F.Column(0))

a.[0] * (sqrt 392.) / (sqrt (v))

let a1 = -2.400977991
let b1 = 3.772966692

(a1 * a1) / ((a1 * a1) + (b1 * b1))
(//pr.out.cov$rotation[1,1]*pr.out.cov$sdev[1]/sqrt(var(ten.genes[,1]))
 -16.70364525) * (sqrt 392.)
[| 0.536875; 0.843661 |]
|> Array.sumBy (fun y -> y * y)
|> sqrt
0.536875 / 0.9999993243
//variables.Transpose() * variables
//
//let inline sLo x1 x2 =
//    let nom = (x1 * x1) + (x2 * x2)
//    (x1 * x1) / nom
//
//sLo -7.7 14.7
//
//PCA.computeOfMatrix (Matrix.cov wordDataSet)
//F.Transpose() TransposeAndMultiply(variables)
//
//
//let adjCenter   = PCA.toAdjustCovariance wordDataSet
//
//let wordPCA     = PCA.compute adjCenter wordDataSet
//
//let transpose = (adjCenter PCA.AdjustmentDirection.Obverse wordDataSet)
//
//        
//let svd            =   transpose.Svd(true)
//let singularValues = svd.W().Diagonal() |> Seq.toArray
//// EigenVectors are the right sigular vectors
//let eigenVectors   =  svd.VT().Inverse()
//
//
////let singularValues = eigenValues |> Seq.map (fun ev -> sqrt ev) |> Seq.toArray
//let lM             = DiagonalMatrix(singularValues.Length,singularValues.Length,singularValues)
//let fsMatrix       = lM * eigenVectors
//
//
//
//let wordDataPCA = PCA.transform adjCenter wordPCA wordDataSet
//
//let wordrev     = PCA.revert adjCenter wordPCA wordDataPCA
//
//PCA.getFeatureMatrixOfComponents wordPCA
//
//covarianceUnbaised 
//
//let covM = Matrix.columnCovariance wordDataSet
//PCA.computeOfCovarianceMatrix covM
//
//let x1 = 0.5368754922
//let x2 = -0.8436614877
//
//let nn = (x1 * x1) + (x2 * x2)
//let n = x1 + x2
//
//4.427188724
