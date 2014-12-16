namespace FSharpBio.Statistics.Descriptive

module Correlation =

    /// Fisher-Z transformation for Pearson correlation coefficient    
    let private transformFisherZ r =
        0.5 * ( log ((1.+r) / (1.-r)) )
        
    /// Standart deviation Fisher-Z transformation for Pearson correlation coefficient
    let private stdFisherZ n =
        1. / sqrt(float(n-3))

    /// Fisher-Z transformation for Pearson correlation coefficient    
    /// after Hotelling (1953) for n< 50
    let private transformFisherHotellingZ r n =
        let hotelling r z = z - ( ( 3.*z + r) / (4.*(n-1.) ) )
        let z = transformFisherZ  r                
        if System.Double.IsNegativeInfinity(z) then
            printfn "Warning: FisherZ out of definition. Value is set to the boarder"            
            hotelling r (-18.71497388)
        elif System.Double.IsPositiveInfinity(z) then
            printfn "Warning: FisherZ out of definition. Value is set to the boarder"
            hotelling r (18.71497388)
        else
            hotelling r (z)                    


    /// Standart deviation Fisher-Z transformation for Pearson correlation coefficient
    /// after Hotelling (1953) for n< 50
    let private stdFisherHotellingZ n =
        if n < 1 then
            //printfn "Parameter warning: not n < 1"
            nan
        else
            1. / sqrt(float(n-1))



                                    
    type CorrelationCoefficient = { 
        /// Pearson r
        Coefficient : float;
        PValue : float;
        ZValue : float; }

    let emptyCorrelationCoefficient = { Coefficient = 0.0; PValue = nan; ZValue = nan; } 

    /// Pearson correlation 
    let pearsonCorrelation dataA dataB = 
        let n = Seq.length dataA
        let cf = MathNet.Numerics.Statistics.Correlation.Pearson(dataA,dataB)
        let z = transformFisherZ cf
        let zStd = stdFisherZ n
        { Coefficient = cf; PValue = (z/zStd); ZValue = z }


    /// Pearson correlation (nan support by JackKnife leave one out)
    //  Biometry third edition R.Sokal / F. Rohlf page. 820
    let pearsonCorrelationNan dataA dataB = 
        let filtered = (Seq.map2 (fun a b -> if System.Double.IsNaN(a) || System.Double.IsNaN(b) then
                                                None
                                             else
                                                Some(a,b) ) dataA dataB ) |> Seq.choose (fun x -> x)
        let n = float(Seq.length(filtered))
        let fdataA,fdataB = filtered |> List.ofSeq |> List.unzip        
        let cf = MathNet.Numerics.Statistics.Correlation.Pearson(fdataA,fdataB)
        let nz = n * (transformFisherHotellingZ cf n)
        
        if n < 3. then 
            //printfn "Parameter warning: not n < 3"
            //[] |> Seq.ofList
            { Coefficient = cf; PValue = nan; ZValue = (transformFisherHotellingZ cf n) }
        
        else
            //printfn "%f" cf
            //Jackknife
            let jackknife =
                filtered             
                |> Seq.mapi (fun leaveOutI v -> filtered |> Seq.mapi (fun i x -> if (i = leaveOutI) then None else Some(x)))
            let pseudoCfs =
                jackknife
                |> Seq.map (fun fdatas -> let fdataA,fdataB = fdatas |> Seq.choose (fun x -> x) |> List.ofSeq |> List.unzip
                                          MathNet.Numerics.Statistics.Correlation.Pearson(fdataA,fdataB) )
        
            let pseudoZs = pseudoCfs |> Seq.map (fun pcf -> (nz - ((n-1.)*(transformFisherHotellingZ pcf (n-1.)))) )
            let pseudoZ  = pseudoZs |> StatisticalMeasure.NaN.median
            let pseudoZStd =sqrt ((pseudoZs |> StatisticalMeasure.NaN.stDevPopulation) / (n-1.))
            let StudentT = new MathNet.Numerics.Distributions.StudentT(0.0,1.0,(n-1.))
            //StudentT.DegreesOfFreedom <- (n-1.)
            //{ Coefficient = cf; PValue = 1.-StudentT.CumulativeDistribution(pseudoZ/pseudoZStd); ZValue = (transformFisherZ cf) }
            //{ Coefficient = cf; PValue = (pseudoZStd); ZValue = (transformFisherHotellingZ cf n) }
            //pseudoCfs
            let q = pseudoZ/pseudoZStd
            if (System.Double.IsNaN(q)) then
                { Coefficient = cf; PValue = 0.0; ZValue = (transformFisherHotellingZ cf n) }            
            else
                { Coefficient = cf; PValue = 1.-StudentT.CumulativeDistribution(q); ZValue = (transformFisherHotellingZ cf n) }            

        // ##########################################################################
        // TEST: of Pearson correlation (nan support by JackKnife leave one out)
        // ################
        //let d1 = [159.;179.;100.;45.;384.;230.;100.;320.;80.;220.;320.;210.;]
        //let d2 = [14.4;15.2;11.3;2.5;22.7;14.9;1.41;15.81;4.19;15.39;17.25;9.52; ]
        //
        //let test = pearsonCorrelationNan d1 d2

    let calcCoefficientMatrix (data:float[][]) (pCoefficientThresholf:float) (pValueThresholf:float) = 
        let length = Array.length data
        Array2D.init length length (fun rowI colI ->    try
                                                            let cor = pearsonCorrelationNan(data.[rowI]) (data.[colI])
                                                            //cor.Coefficient)
                                                            if(cor.PValue<=pValueThresholf && cor.Coefficient >= pCoefficientThresholf) then cor.Coefficient else 0. 
                                                        with
                                                            | _ -> raise (System.NotImplementedException((sprintf "rowI: %i colI: %i" rowI colI)) ))  