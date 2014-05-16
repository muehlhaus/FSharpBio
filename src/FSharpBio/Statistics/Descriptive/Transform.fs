namespace FSharpBio.Statistics.Descriptive

open System

module Transform =

    module PropagationOfError =

        /// Returns the relative error
        let inline toRelativeError (x,xDiv) =
            xDiv / x

        /// Returns the absulute error
        let inline toAbsoluteError (x,xDiv) =
            xDiv * x

        /// Linear propagation of error for z = x + y or z = x - y
        //  standart deviation as one possible estimation for the uncertainty of measurement
        //  returns absolute error deltaZ
        let linearSumDif (xm,xStd) (ym,yStd)  = 
            xStd + yStd

        /// Linear propagation of error for z = x * y or z = x / y
        //  standart deviation as one possible estimation for the uncertainty of measurement
        //  returns relative error deltaZ/z
        let linearProdDiv (xm,xStd) (ym,yStd)  = 
            abs(xStd / xm) + abs(yStd / ym)

        /// Linear propagation of error for z = x^a * y^b 
        //  standart deviation as one possible estimation for the uncertainty of measurement
        //  returns relative error deltaZ/z
        let linearPower (xm,xStd,xExponent) (ym,yStd,yExponent)  = 
            abs(xStd / xm * xExponent) + abs(yStd / ym * yExponent)


        /// Gaussian propagation of error for z = x + y or z = x - y
        //  standart deviation as one possible estimation for the uncertainty of measurement
        //  returns absolute error deltaZ
        let gaussSumDif (xm,xStd) (ym,yStd)  = 
            sqrt (xStd**2. + yStd**2.)

        /// Gaussian propagation of error for z = x * y or z = x / y
        //  standart deviation as one possible estimation for the uncertainty of measurement
        //  returns relative error deltaZ/z
        let gaussProdDiv (xm,xStd) (ym,yStd)  = 
            sqrt ((xStd / xm)**2. + (yStd / ym)**2.)

        /// Gaussian propagation of error for z = x^a * y^b 
        //  standart deviation as one possible estimation for the uncertainty of measurement
        //  returns relative error deltaZ/z
        let gaussPower (xm,xStd,xExponent) (ym,yStd,yExponent)  = 
            sqrt ((xStd / xm * xExponent)**2. + (yStd / ym * yExponent)**2.)

    //  Implemented according to publication:
    //  Milligan GW, CooperM (1988) : A Study of Standardization of Variables in Cluster Analysis (Journal of Classification 5:181-204)
    /// Standardization of varables 
    module Standardization =
        
        
                
        /// Values are standardized to z scores, with a mean of 0 and a standard deviation of 1
        /// Z1 = (X - (mean(x)) / sd        
        let zScore (a:array<float>) =
            let mean = StatisticalMeasure.mean a                    
            let sd = StatisticalMeasure.stDevPopulation a            
            a
            |> Array.map ( fun x -> (x - mean) / sd)
        
        //  Remark: The comparative location of the score remains
        /// Values are standardized by dividing by standard deviation
        /// Z2 = X / sd 
        /// mean = mean (x) / sd
        let byStandardDeviation (a:array<float>) =              
            let sd = StatisticalMeasure.stDevPopulation a
            a
            |> Array.map ( fun x -> (x / sd))

        // Comment regarding clustering:  As Z1 and Z2 are linear functions of each other, the result is the same in euclidian space


        /// Z3 = X / MAX(X)
        let byMax (proportionalityConstant:float) (a:array<float>) =              
            let max = (a |> Array.maxBy (fun x -> x + proportionalityConstant)) + proportionalityConstant
            a
            |> Array.map ( fun x -> ((x + proportionalityConstant) / max))

        /// Z4 = X / (MAX(X) - MIN(X))
        let byRange (proportionalityConstant:float) (a:array<float>) =              
            let max = (a |> Array.maxBy (fun x -> x + proportionalityConstant)) + proportionalityConstant
            let min = (a |> Array.minBy (fun x -> x + proportionalityConstant)) + proportionalityConstant
            let range = max - min
            a
            |> Array.map ( fun x -> ((x + proportionalityConstant) / range)) 
        
        /// For non negative numbers bounbded by 0.0 and 1.0
        /// Z5 = (X - MIN(X)) / (MAX(X) - MIN(X))
        let boundedByRange (proportionalityConstant:float) (a:array<float>) =              
            let max = (a |> Array.maxBy (fun x -> x + proportionalityConstant)) + proportionalityConstant
            let min = (a |> Array.minBy (fun x -> x + proportionalityConstant)) + proportionalityConstant
            let range = max - min
            a
            |> Array.map ( fun x -> ((x + proportionalityConstant - min) / range)) 

        /// Z6 = (X - MIN(X)) / (MAX(X) - MIN(X))
        let bySum (a:array<float>) =              
            let sum = a |> Array.sum                        
            a
            |> Array.map ( fun x -> (x  / sum))

        /// Z7 = Rank(X)
        let rank (rankingFunction: seq<float> -> seq<Rank.RankedValue<float>> ) (a:array<float>) = 
            a |> rankingFunction
            |> Seq.map (fun rv -> rv.RankIndex)
            |> Seq.toArray




    let rev_ln (a:float[]) =
        a |> Array.map exp

    let rev_lnStd (a:float[]) =
        a |> Array.map (fun x -> (exp x) - 1.)

    let toRelativeError (data:float[]) (std:float[]) =
        (Array.map2 (fun x dx -> dx / x ) data std)

    let toAbsoluteError (data:float[]) (std:float[]) =
        (Array.map2 (fun x dx -> dx * x ) data std)

    /// Transform a value x from space Q = [A, B] to value x' in space Q' = [A', B'] 
    /// (where f(A) = B' and f(B) = A')
    let linearRangeOfValue (lowerUpper:float*float) (newLowerNewUpper:float*float) (x:float) = 
        let A,B = lowerUpper
        let A',B' = newLowerNewUpper
        let k = (B - A) / (B' - A')
        (x - A) / k + A'
    

    /// Transform all values xi of an array a from space Q = [min(a), max(a)] to value x' in space Q' = [A', B'] 
    /// (where f(A) = B' and f(B) = A')
    let linearRange (newLowerNewUpper:float*float) (a:array<float>) = 
        let rg = StatisticalMeasure.range a
        a |> Array.map (fun x -> linearRangeOfValue (rg.Min, rg.Max) (newLowerNewUpper) x )


    /// Linear transformation to a new mean and standard deviation
    let linearMeanStd (newMean:float) (newStd:float) (a:array<float>) =
        let mean = MathNet.Numerics.Statistics.Statistics.Mean(a)    
        let sd = MathNet.Numerics.Statistics.Statistics.StandardDeviation(a)
        a
        |> Array.map ( fun x -> (x - mean) / sd * newStd + newMean)
    
    
    
    /// Centers the data around their mean (xi - mean(x))
    let center2Mean (a:array<float>) =
        let mean = MathNet.Numerics.Statistics.Statistics.Mean(a)            
        a
        |> Array.map ( fun x -> (x - mean))


    //  Ref.: Box, G. E. P. and Cox, D. R. (1964), An Analysis of Transformations, Journal of the Royal Statistical Society, p211-243, discussions on p244-252.
    /// Box Cox transformation is used to convert data so that they are normally distributed
    /// Critical part: finding lamda
    let boxCox (lamda:float) (a:array<float>) =
        if lamda = 0. then
            a |> Array.map (fun x -> log(x))
        else
            a |> Array.map (fun x -> (x**lamda - 1.) / lamda)

    let foldChange  (a:array<float>) =        
        let firstValue = a.[0]
        a
        |> Array.map ( fun x -> (x / firstValue))


    let foldChangeInLogSpace  (a:array<float>) =        
        let firstValue = a.[0]
        a
        |> Array.map ( fun x -> (x - firstValue))


    let foldChangeErr (values:array<float>) (sdevs:array<float>) =        
        let firstValue = values.[0]  //**2.
        let firstDev   = sdevs.[0]   //**2.       
        //Array.map2 ( fun x sdx -> ( (1.0/firstValue)*((1.0/(sdx**2.0)) + (x**2.0/sdx**2.0))*(1.0/firstDev) )) values sdevs
        let a = Array.map2 ( fun x sdx -> PropagationOfError.linearProdDiv (x,sdx) (firstValue,firstDev)) values sdevs
        a.[0] <- firstDev / firstValue
        a


    let percentMax (a:array<float>) =        
        let maxValue = a |> Array.max
        a
        |> Array.map ( fun x -> ((x / maxValue) * 100.))

    /// Calculate percent change
    /// % change = (v2 - v1 / v1) * 100
    /// (-) decrease (+) increase
    let percentChange (a:array<float>) =        
        a 
        |> Seq.windowed 2 
        |> Seq.map (fun v -> ((v.[1] - v.[0]) / v.[0]) * 100. ) 
        |> Seq.toArray

    let movingAverage (period : int) (values : float seq) =
        Seq.zip values (Seq.skip period values)
        |> Seq.scan (fun last (prev, cur) -> last - prev + cur) (values |> Seq.take period |> Seq.sum)
        |> Seq.map (fun x -> x / float(period))

    
