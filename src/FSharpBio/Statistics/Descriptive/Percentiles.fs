//##############################################################
// A Wrapper module for Mathnet Numerics Percentile class

namespace FSharpBio.Statistics.Descriptive

open FSharp.CoreX

module Percentiles = 
                      

    /// Computes percentile
    /// percentile: The percentile must be between 0.0 and 1.0 (inclusive)
    let computePercentile (calcMethod) (percentile:float) (data:seq<float>) =
        MathNet.Numerics.Statistics.Statistics.QuantileCustom(data,percentile,calcMethod)
        

    /// Computes percentiles
    /// percentiles: Each percentile must be between 0.0 and 1.0 (inclusive)
    let computePercentiles (calcMethod) (percentile:seq<float>) (data:seq<float>) =
        let qtf = Interop.ofFunc (MathNet.Numerics.Statistics.Statistics.QuantileCustomFunc(data,calcMethod))
        percentile |> Seq.map qtf

    
    /// Computes the interquartile range (IQR)
    //  The IQR is the 1st Quartile subtracted from the 3rd Quartile; these quartiles can be clearly seen on a box plot on the data.
    //  It is a trimmed estimator, defined as the 25% trimmed mid-range, and is the most significant basic robust measure of scale.
    let IQR (calcMethod)  (data:seq<float>)=
        let ptf = Interop.ofFunc (MathNet.Numerics.Statistics.Statistics.PercentileFunc(data))        
        (ptf 75) - (ptf 25)
        



//    /// Calculates empitical cumulative density function
//    let ecdf (data:seq<float>) =
//        let perc =  MathNet.Numerics.Statistics.Percentile(data)
//        perc.Method <- MathNet.Numerics.Statistics.PercentileMethod.Nist
//        let points = [0.0..0.01..1.]
//        let cperc = perc.Compute(points)
//        Seq.map2 (fun p cp -> (cp,p)) points cperc

