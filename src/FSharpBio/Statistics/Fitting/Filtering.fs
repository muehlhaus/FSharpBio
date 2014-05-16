namespace FSharpBio.Statistics.Fitting

module Filtering =

    let toComplexFloat (d:float) = 
        System.Numerics.Complex(d,0.)

    let fromComplexFloat (c:System.Numerics.Complex) = 
        c.Real

    let toComplexFloatArray (data:seq<float>) =
        data 
        |> Seq.map toComplexFloat
        |> Seq.toArray
    
    let fromComplexFloatArray (data:System.Numerics.Complex []) =
        data 
        |> Array.map fromComplexFloat

    //http://www.centerspace.net/blog/nmath/iir-filtering-with-butterworth-filters/
    let ButterworthFilter  (sampleFrequency : float) (order : int) (f0 : float) (dcGain : float) (signal:seq<float>) =
        
        let signalFFT = toComplexFloatArray signal
        MathNet.Numerics.IntegralTransforms.Transform.FourierForward(signalFFT)
  
        let n       = signalFFT.Length       
        let numBins = float n / 2.  // Half the length of the FFT by symmetry
        let binWidth = sampleFrequency / numBins // Hz
        // Filter
        for i = 1 to n / 2 do
            let binFreq = binWidth * float i
            let gain = dcGain / ( sqrt( ( 1. + System.Math.Pow ( binFreq / f0, 2.0 * float order ) ) ) ) |> toComplexFloat
            signalFFT.[i] <- signalFFT.[i] * gain
            signalFFT.[n - i] <- signalFFT.[n - i] * gain

        // Reverse filtered signal
        MathNet.Numerics.IntegralTransforms.Transform.FourierInverse(signalFFT)
        signalFFT |> fromComplexFloatArray
        
    
    ///http://www.centerspace.net/blog/nmath/chebyshev-filters-with-nmath/
    let ChebeshevFilter = 
        0

    