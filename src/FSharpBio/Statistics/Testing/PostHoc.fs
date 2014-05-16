namespace FSharpBio.Statistics.Testing

module PostHoc =    

    open FSharpBio.Statistics.Descriptive
    open FSharpBio.Statistics.Testing.TestStatistics    
    
    type Contrast = { Index            : int;                      
                      L                : float;
                      DegreesOfFreedom : float;
                      MeanSquares      : float;
                      Significance     : float;                      
                      Statistic        : float;
                      SumOfSquares     : float;
                      }
    
    let createContrast index l degreesOfFreedom meanSquares significance statistic sumOfSquares =
        {Index = index; L = l; DegreesOfFreedom = degreesOfFreedom; MeanSquares = meanSquares; Significance = significance; Statistic = statistic; SumOfSquares = sumOfSquares;}

    let private sumOfSquareContrast (sampleSizes:int[]) (sampleMeans:float[]) (contrast:float[]) =        
        let l           =  Array.fold2 (fun state mi ai -> state + (mi * ai)) 0.0 sampleMeans contrast 
        let denominator = (Array.map2 (fun a n -> a * a / (float n)) contrast sampleSizes) |> Array.sum
        (l * l / denominator,l)
    

    let Scheffe (contrastMatrix:float[][]) (data:float[][]) =
        
        // Sample sizes
        let sizes = data |> Array.map (fun x -> x.Length)
        let totalSize = sizes |> Array.sum
        let groupCount = data.Length
        // Degrees of freedom
        let Db = float(groupCount - 1)
        let Dw = float(totalSize - groupCount)
        let Dt = groupCount * totalSize - 1

        // Step 1. Calculate the mean within each group
        let sampleMeans = data |> Array.map (fun x -> StatisticalMeasure.mean(x))        
        // Step 2. Calculate the sum of squares contrast associated
        let ssc_l = contrastMatrix |> Array.map (fun ar -> sumOfSquareContrast sizes sampleMeans ar)
        let ssc = ssc_l |> Array.map (fun (ssc,l) -> ssc)
        let l = ssc_l |> Array.map (fun (ssc,l) -> l)
        // Step 3. Calculate the "within-group" sum of squares
        let Sw = data|> Array.mapi (fun i ar -> ar |> Array.fold (fun acc elem -> acc + ((elem-sampleMeans.[i])**2.0)) 0.0) |> Array.sum
        let MSw = Sw / Dw // within-group mean square or MSerror
        // Step 5. Calculate the F statistic per contrast
        Array.mapi2 (fun i sscV l' -> let fValue = Db * sscV / MSw
                                      if nan.Equals(fValue) then
                                        let FTest = new FTEST(fValue, Db, Dw)
                                        createContrast i l' Db MSw FTest.PValue FTest.Statistic sscV
                                      else
                                        createContrast i l' Db MSw nan nan sscV  
                    ) ssc l
                

    
    //
    let Bonferroni (contrastMatrix:float[][]) (data:float[][]) =
        
        // Sample sizes
        let sizes = data |> Array.map (fun x -> x.Length)
        let totalSize = sizes |> Array.sum
        let groupCount = data.Length
        // Degrees of freedom
        let Db = float(groupCount - 1)
        let Dw = float(totalSize - groupCount)
        let Dt = groupCount * totalSize - 1
        // Number of contrasts
        let c' = float contrastMatrix.Length

        // Step 1. Calculate the mean within each group
        let sampleMeans = data |> Array.map (fun x -> StatisticalMeasure.mean(x))        
        // Step 2. Calculate the sum of squares contrast associated
        let ssc_l = contrastMatrix |> Array.map (fun ar -> sumOfSquareContrast sizes sampleMeans ar)
        let ssc = ssc_l |> Array.map (fun (ssc,l) -> ssc)
        let l = ssc_l |> Array.map (fun (ssc,l) -> l)
        // Step 3. Calculate the "within-group" sum of squares
        let Sw = data|> Array.mapi (fun i ar -> ar |> Array.fold (fun acc elem -> acc + ((elem-sampleMeans.[i])**2.0)) 0.0) |> Array.sum
        let MSw = Sw / Dw // within-group mean square or MSerror
        // Step 5. Calculate the F statistic per contrast
        Array.mapi2 (fun i sscV l' -> let fValue = sscV / MSw
                                      if nan.Equals(fValue) then
                                        let FTest = new FTEST(fValue, 1., Dw)
                                        createContrast i l' Db MSw (FTest.PValue * c') FTest.Statistic sscV
                                      else
                                        createContrast i l' Db MSw nan nan sscV  
                    ) ssc l
        
        
    // Ref.: Hays, William L. (1988). Statistics (4th ed.). New York: Holt, Rinehart, & Winston. (pp. 313–315)                                      
    let Hays (contrastMatrix:float[][]) (data:float[][]) =
        
        // Sample sizes
        let sizes = data |> Array.map (fun x -> x.Length)
        let totalSize = sizes |> Array.sum
        let groupCount = data.Length
        // Degrees of freedom
        let Db = float(groupCount - 1)
        let Dw = float(totalSize - groupCount)
        let Dt = groupCount * totalSize - 1
        

        // Step 1. Calculate the mean within each group
        let sampleMeans = data |> Array.map (fun x -> StatisticalMeasure.mean(x))        
        // Step 2. Calculate the sum of squares contrast associated
        let ssc_l = contrastMatrix |> Array.map (fun ar -> sumOfSquareContrast sizes sampleMeans ar)
        let ssc = ssc_l |> Array.map (fun (ssc,l) -> ssc)
        let l = ssc_l |> Array.map (fun (ssc,l) -> l)
        // Step 3. Calculate the "within-group" sum of squares
        let Sw = data|> Array.mapi (fun i ar -> ar |> Array.fold (fun acc elem -> acc + ((elem-sampleMeans.[i])**2.0)) 0.0) |> Array.sum
        let MSw = Sw / Dw // within-group mean square or MSerror
        // Step 5. Calculate the F statistic per contrast
        Array.mapi2 (fun i sscV l' -> let fValue = sscV / MSw
                                      printfn  "%f %b " fValue (nan.Equals(fValue))
                                      if nan.Equals(fValue) then
                                        createContrast i l' Db MSw nan nan sscV 
                                      else
                                        let FTest = new FTEST(fValue, 1., Dw)
                                        createContrast i l' Db MSw FTest.PValue FTest.Statistic sscV                                      
                                         
                    ) ssc l






    