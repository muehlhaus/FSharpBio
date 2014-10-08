namespace FSharpBio.Mz


//  ################################################################################
//  Paper: Target-decoy search strategy for increased confidence in large-scale protein identifications by mass spectrometry - Nat Methods 2007
/// Module for calculation of empirical target- decoy measures 
module EmpiricalTargetDecoy = 

    // 
    type TaggedItem<'a,'b> = {
        Tag  : 'a
        Item : 'b
        }
    let createTaggedItem tag item =
        {Tag = tag; Item = item}
    
    type TargetDecoyTag =
        | Target
        | Decoy

    let isTarget (tag:TargetDecoyTag) =
        match tag with
        | Target -> true
        | Decoy  -> false

    let isDecoy (tag:TargetDecoyTag) =
        match tag with
        | Target -> false
        | Decoy  -> true

    /// Taget - Decoy Measure (see: Nat Methods 2007)
    type TargetDecoyMeasure<'a> = { FalsePositive : int
                                    TruePositive  : int
                                    FalseNegative : int
                                    TrueNegative  : int
                                    Precision     : float
                                    FPR           : float
                                    Sensitivity   : float
                                    Specificity   : float
                                    Accuracy      : float
                                    Value         : 'a
                                    }

    
    /// Creates a TargetDecoyMeasure record type
    let createTargetDecoyMeasure falsePositive truePositive falseNegative trueNegative precision fpr sensitivity specificity accuracy value =
        { FalsePositive = falsePositive; TruePositive = truePositive; FalseNegative = falseNegative;
            TrueNegative = trueNegative; Precision = precision; FPR = fpr; Sensitivity = sensitivity; 
            Specificity = specificity; Accuracy = accuracy; Value = value; }


        
    
    let calculateSingleMeasure (p0) value (targetCount:float) (decoyCount:float) (totalTargetCount:float) (totalDecoyCount:float) =
        let totalN = totalTargetCount + totalDecoyCount
    
        let fp = decoyCount * p0
        let tp = targetCount + decoyCount - fp

        let tc = totalTargetCount
        let ti = totalDecoyCount
        let fn = tc - tp
        let tn = ti - fn

        let precision = tp / (tp + fp)
        let fpRate = 1. - precision
        let sensitivity = tp / tc
        let specificity = tn  / (tn + fp)
        let accuracy = (tp + tn) / totalN

        createTargetDecoyMeasure (int fp) (int tp) (int fn) (int tn) precision fpRate sensitivity specificity accuracy value


    /// Calculates empirical target-decoy meassures for each score 
    /// Attention: FDR is global empirical FDR
    let calculate (p0) (data:TaggedItem<TargetDecoyTag,'b> list) = 
        let sData = data |> List.sortBy (fun d -> d.Item) |> List.rev
        let targets,decoys = data |>  List.partition (fun d -> isTarget d.Tag)
        let totalTargetCount = float targets.Length
        let totalDecoyCount = float decoys.Length

        let rec go (l:TaggedItem<TargetDecoyTag,'b> list) (targetCount:float) (decoyCount:float) (result: TargetDecoyMeasure<'b> list) =
            match l with
            | []      -> result
            | f::rest -> let nTargetCount = if isTarget f.Tag then (targetCount + 1.) else targetCount
                         let nDecoyCount  = if isDecoy f.Tag then (decoyCount + 1.) else decoyCount
                         match rest with
                         | []      -> let tdr = calculateSingleMeasure p0 f.Item nTargetCount nDecoyCount totalTargetCount totalDecoyCount
                                      tdr::result
                         | s::rest -> if f.Item <> s.Item then
                                         let tdr = calculateSingleMeasure p0 f.Item nTargetCount nDecoyCount totalTargetCount totalDecoyCount
                                         go (s::rest) (nTargetCount) (nDecoyCount) (tdr::result) 
                                      else
                                         go (s::rest) (nTargetCount) (nDecoyCount) (result) 
        

        go sData 0. 0. []