namespace FSharpBio.Statistics.Descriptive

module ProbStats =


    // Calculates tukey bi-weighted average
    let TukeyAverage x =
        let c = 5.
        let epsilon = 1e-04
        let m = StatisticalMeasure.median (x)
        let s = StatisticalMeasure.median ( x |> Seq.map (fun xv -> abs(xv - m)) )
    
        let calcWeight xv =
            let u = (xv - m) / (c * s + epsilon)
            let w = if (abs(u) <= 1.) then (1. - u**2.)**2. else 0. 
            w
        let w = x |> Seq.map calcWeight
        let sumXW = (Seq.map2 (fun xv wv -> wv * xv) x w)  |> Seq.sum
        sumXW / Seq.sum w

