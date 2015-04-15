namespace FSharpBio.Statistics.Signal

module Filtering =
    
    open FSharp.CoreX
    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra

    //http://www.centerspace.net/blog/nmath/iir-filtering-with-butterworth-filters/
    let butterworthFilter  (sampleFrequency : float) (order : int) (f0 : float) (dcGain : float) (signal:seq<float>) =
        
        let signalFFT = Math.Complex.toComplexFloatArray signal
        MathNet.Numerics.IntegralTransforms.Fourier.Forward signalFFT
  
        let n       = signalFFT.Length       
        let numBins = float n / 2.  // Half the length of the FFT by symmetry
        let binWidth = sampleFrequency / numBins // Hz
        // Filter
        for i = 1 to n / 2 do
            let binFreq = binWidth * float i
            let gain = dcGain / ( sqrt( ( 1. + System.Math.Pow ( binFreq / f0, 2.0 * float order ) ) ) ) |> Math.Complex.toComplexFromReal
            signalFFT.[i] <- signalFFT.[i] * gain
            signalFFT.[n - i] <- signalFFT.[n - i] * gain

        // Reverse filtered signal
        MathNet.Numerics.IntegralTransforms.Fourier.Inverse signalFFT
        signalFFT |> Math.Complex.fromComplexFloatArray
        
    
    ///http://www.centerspace.net/blog/nmath/chebyshev-filters-with-nmath/
    let chebeshevFilter = 
        0

    /// Smooth (and optionally differentiate) data with a Savitzky-Golay filter.
    /// The Savitzky-Golay filter is a type of low-pass filter and removes high frequency noise from data.
    let savitzky_golay (window_size:int) (order:int) deriv rate (data:float[]) =
        if window_size % 2 <> 1 || window_size < 1 then
            failwith "window_size size must be a positive odd number"
        if window_size < order + 2 then
            failwith "window_size is too small for the polynomials order"
        //let order_range = [0..order]
        let half_window = (window_size - 1) / 2
        // precompute coefficients
        let b = [|for colI=0 to order do
                    for k= -half_window to half_window do  yield float(k)**float(colI)|]
                |> DenseMatrix.raw (half_window*2 + 1) (order+1)
    
        let m = (pseudoInvers b).Row(deriv) * ((float(rate)**float(deriv)) * SpecialFunctions.Factorial(deriv))
        //pad the signal at the extremes with values taken from the signal itself
        let firstvals = 
            data.[1..(half_window + 1)]
            |> Array.rev
            |> Array.map (fun d -> (abs d) - data.[0])
            |> Array.map (fun x -> data.[0] - x ) 
        let lastvals = 
            data.[(data.Length - half_window - 1)..data.Length-2]
            |> Array.rev
            |> Array.map (fun d -> (abs d) - data.[data.Length-1])
            |> Array.map (fun x -> data.[data.Length-1] - x )
        let y = Array.concat [firstvals; data; lastvals;] |> DenseVector.raw
        //convolve_valid m  y ?? not sure ??
        
        //correlate_valid m y
        0
