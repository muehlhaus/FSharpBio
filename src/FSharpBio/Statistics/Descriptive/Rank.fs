namespace FSharpBio.Statistics.Descriptive

open FSharp.CoreX

/// Module to Calculate the rank. The rank of a number is its size relative to other values in a sequence
module Rank = 
    /// RanlkedValue type contains the original value (Value), its rank (RankIndex) and its original position (Position)
    type RankedValue<'a> = 
        { Value : 'a
          RankIndex : float
          Position : int }
    
    /// Calculates the rank in ascending order
    let private distinct data = 
        data
        |> Seq.mapi (fun i x -> 
               { Value = x
                 RankIndex = nan
                 Position = i })
        |> Seq.sortBy (fun rv -> rv.Value)
        |> Seq.mapi (fun r rv -> 
               ({ Value = rv.Value
                  RankIndex = float (r + 1)
                  Position = rv.Position }))
    
    /// Returns the RankedValue of a number in a sequence of numbers.
    /// Remark: breaks ties by mean
    let breakByMean data = 
        data
        |> distinct
        |> Seq.groupBy (fun rv -> rv.Value)
        |> Seq.map (fun (v, group) -> 
               let values = group |> Seq.map (fun x -> x.RankIndex)
               let byAverage = values |> StatisticalMeasure.mean
               group |> Seq.map (fun rv -> 
                            { Value = v
                              RankIndex = byAverage
                              Position = rv.Position }))
        |> Seq.concat
        //|> Seq.sortBy (fun rv -> rv.Position)
    
    /// Returns the RankedValue of a number in a sequence of numbers.
    /// Remark: breaks ties by using maximum value
    let breakByMax data = 
        data
        |> distinct
        |> Seq.groupBy (fun rv -> rv.Value)
        |> Seq.map (fun (v, group) -> 
               let values = group |> Seq.map (fun x -> x.RankIndex)
               let byMax = values |> Seq.max
               group |> Seq.map (fun rv -> 
                            { Value = v
                              RankIndex = byMax
                              Position = rv.Position }))
        |> Seq.concat
        //|> Seq.sortBy (fun rv -> rv.Position)
    
    /// Returns the RankedValue of a number in a sequence of numbers.
    /// Remark: breaks ties by using minimum value
    let breakByMin data = 
        data
        |> distinct
        |> Seq.groupBy (fun rv -> rv.Value)
        |> Seq.map (fun (v, group) -> 
               let values = group |> Seq.map (fun x -> x.RankIndex)
               let byMin = values |> Seq.min
               group |> Seq.map (fun rv -> 
                            { Value = v
                              RankIndex = byMin
                              Position = rv.Position }))
        |> Seq.concat
        //|> Seq.sortBy (fun rv -> rv.Position)
    
    //Calculates the rank in descending order
    let private distinctDesc data = 
        data
        |> Seq.mapi (fun i x -> 
               { Value = x
                 RankIndex = nan
                 Position = i })
        |> Seq.sortByDesc (fun rv -> rv.Value)
        |> Seq.mapi (fun r rv -> 
               ({ Value = rv.Value
                  RankIndex = float (r + 1)
                  Position = rv.Position }))
    
    /// Returns the RankedValue of a number (ascending order) in a sequence of numbers.
    /// Remark: breaks ties by mean    
    let breakByMeanDesc data = 
        data
        |> distinctDesc
        |> Seq.groupBy (fun rv -> rv.Value)
        |> Seq.map (fun (v, group) -> 
               let values = group |> Seq.map (fun x -> x.RankIndex)
               let average = values |> StatisticalMeasure.mean
               group |> Seq.map (fun rv -> 
                            { Value = v
                              RankIndex = average
                              Position = rv.Position }))
        |> Seq.concat
        //|> Seq.sortBy (fun rv -> rv.Position)
    
    /// Returns the RankedValue of a number (ascending order) in a sequence of numbers.
    /// Remark: breaks ties by using maximum value
    let breakByMaxDesc data = 
        data
        |> distinctDesc
        |> Seq.groupBy (fun rv -> rv.Value)
        |> Seq.map (fun (v, group) -> 
               let values = group |> Seq.map (fun x -> x.RankIndex)
               let byMax = values |> Seq.max
               group |> Seq.map (fun rv -> 
                            { Value = v
                              RankIndex = byMax
                              Position = rv.Position }))
        |> Seq.concat
        //|> Seq.sortBy (fun rv -> rv.Position)

    /// Returns the RankedValue of a number (ascending order) in a sequence of numbers.
    /// Remark: breaks ties by using minimum value    
    let breakByMinDesc data = 
        data
        |> distinctDesc
        |> Seq.groupBy (fun rv -> rv.Value)
        |> Seq.map (fun (v, group) -> 
               let values = group |> Seq.map (fun x -> x.RankIndex)
               let byMin = values |> Seq.min
               group |> Seq.map (fun rv -> 
                            { Value = v
                              RankIndex = byMin
                              Position = rv.Position }))
        |> Seq.concat
        //|> Seq.sortBy (fun rv -> rv.Position)
