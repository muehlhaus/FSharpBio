namespace FSharpBio.Statistics.Fitting


module CrossValidation =

    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.LinearAlgebra.Double
    
    open FSharpBio.Statistics.Descriptive
    open FSharpBio.Statistics.Descriptive.StatisticalMeasure
    
    
    type trainingFunc<'a> = float[] -> float[] -> 'a -> float -> float
        

    /// Creates a  classifiers using training sets that are bootstrapped (drawn with replacement)
    let bagging (f: trainingFunc<'a>) xData yData nIterations (rho:'a) =
        let xyData    = Array.zip xData yData
        let bootSizeK = xyData.Length / 2
        let m         = bootSizeK *  3    
        let predY =
            [ for i = 1 to nIterations do
                let nxData,nyData = Bootstrap.sampleWithReplacement xyData bootSizeK |> Array.unzip                
                let trainingF = f nxData nyData rho
                yield (xData |> Seq.map trainingF) ]
                                                
        let mData = DenseMatrix.OfColumns (xData.Length,nIterations,predY)
        let rowMeans = Matrix.rowMean mData// mData.RowEnumerator() |> Seq.map (fun (i,row) -> StatisticalMeasure.mean row)
        mData.EnumerateColumns() |> Seq.averageBy (fun col -> (StatisticalMeasure.UtilityFunctions.sumOfSquares col rowMeans) / float (xData.Length - 1))


    let optimize rhoS (modelBias:float[]) (modelVariance:float[]) =
        let lower = rhoS |> Seq.min
        let upper = rhoS |> Seq.max

        let akimaBias = MathNet.Numerics.Interpolation.CubicSpline.InterpolateAkima(rhoS |> Seq.toArray,modelBias)
        let akimaVariance = MathNet.Numerics.Interpolation.CubicSpline.InterpolateAkima(rhoS |> Seq.toArray,modelVariance)
        let optFunction = new System.Func<float,float>( fun x -> (akimaBias.Differentiate x) - (akimaVariance.Differentiate x) )
                
        MathNet.Numerics.RootFinding.Brent.FindRoot(optFunction, lower, upper)

//    let b (f: trainingFunc<'a>) (rhoS:float[]) =        
//        let bias = rhoS |> Seq.map (fun rho -> let info,s,rep = alglib.spline1dfitpenalized(xdata, ydata, ( ydata.Length), rho)
//                                               (rep.avgerror)) |> Seq.toArray // RMS <=> Bias^2
//        let variance = rhoS |> Seq.map (fun rho -> ((baggingCV xdata ydata 50 rho))) |> Seq.toArray
//
//        let akimaBias = MathNet.Numerics.Interpolation.Algorithms.AkimaSplineInterpolation(rhoS |> Seq.toArray,bias)
//
//
//        let akimaVariance = MathNet.Numerics.Interpolation.Algorithms.AkimaSplineInterpolation(rhoS |> Seq.toArray,variance)
//
//
//        let test = new System.Func<float,float>( fun x -> (akimaBias.Differentiate x) - (akimaVariance.Differentiate x) )
