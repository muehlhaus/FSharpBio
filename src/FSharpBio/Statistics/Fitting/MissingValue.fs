namespace FSharpBio.Statistics.Fitting


/// Module for missing value imputation
module MissingValue = 
     
    open MathNet.Numerics.LinearAlgebra.Double
    open MathNet.Numerics.LinearAlgebra.Generic
    open FSharp.CoreX
    open FSharpBio.Statistics.Descriptive    


    module DataFilter =

        let reduceByMinNRep (nRep:int) (data:list<list<'a>>) =
     
            let rec mFilter2 (dataL:list<list<'a>>) (nDataL:list<list<'a>>) =
                match dataL with
                | []    -> nDataL |> List.rev
                | h::t  -> if h.Length >= nRep then 
                                mFilter2 t (h::nDataL)
                            else
                                mFilter2 t (nDataL)        

            mFilter2 data []



        let existLessThenCountsOfMissingValues (maxCount:int) (data:seq<#seq<'a>>) = 
            let cStats = data   |> Seq.map Seq.length
            let count  = cStats |> Seq.filter (fun x -> x = 0) |> Seq.length
            if count < maxCount then
                true
            else
                false
    

        let existLessThenCountsOfNRep (nRep:int) (maxCount:int) (data:seq<#seq<'a>>) = 
            let cStats = data |> Seq.map Seq.length
            let count  = cStats |> Seq.filter (fun x -> x > 0 && x < nRep) |> Seq.length
            if count < maxCount then
                 true
            else
                false    


        /// Checks if at all given position indices data contain minimal n number of object counts
        /// Returns true or false
        let exists_minObjectCountsAt (minCount:int) (atPosIndices:int list) (data:seq<#seq<'a>>) = 
            let cStats = data |> Seq.map Seq.length |> Seq.toArray
            let rec checkIf (at: int list) (cStats:int array) =
                match at with
                | []   -> true
                | h::t -> if cStats.[h] >= minCount then
                            checkIf t cStats
                          else
                            false 
            if atPosIndices.Length > 0 then checkIf atPosIndices cStats else raise (System.Exception("Index list can not be empty."))




    
    /// Replace +Infinity with max value and -Infinity with min value in matrix        
    let replaceInfinityValues (m:Matrix<float>) =        
        if ( m |> Matrix.exists (fun x -> System.Double.IsInfinity(x) ) ) && ( m |> Matrix.exists (fun x -> not(System.Double.IsNaN(x) || System.Double.IsInfinity(x) ) ) ) then
                //replace inf
                let max = m |> Matrix.toArray2
                            |> Array2D.array2D_to_seq 
                            |> Seq.filter (fun x -> not(System.Double.IsInfinity(x) ) && not(System.Double.IsNaN(x)) ) 
                            |> Seq.max
                let min = m |> Matrix.toArray2
                            |> Array2D.array2D_to_seq 
                            |> Seq.filter (fun x -> not(System.Double.IsInfinity(x) ) && not(System.Double.IsNaN(x)) ) 
                            |> Seq.min
                let nm =  m |> Matrix.map (fun x -> if System.Double.IsPositiveInfinity(x) then max elif  System.Double.IsNegativeInfinity(x) then min else x)    
                nm
        else
            m


    
    /// Eeplaces NaNs in matrix by sampling from a normal distribution over columns    
    let fillByRandomSampling (m:Matrix<float>) =    
        let getNormalSample (data:float[]) =
            let mean = StatisticalMeasure.NaN.median data 
            let std  = StatisticalMeasure.NaN.stDevPopulation data
            if not(System.Double.IsNaN(mean) || System.Double.IsNaN(std)) then
                let sample = new MathNet.Numerics.Distributions.Normal(mean, std)
                sample.Samples()
            else
                Seq.initInfinite (fun _ ->  nan)

        if ( m |> Matrix.exists (fun x -> System.Double.IsNaN(x) ) ) then        
            [0..m.ColumnCount-1] 
            |> Seq.iter ( fun i -> if (  m.Column(i) |> Vector.exists (fun x -> System.Double.IsNaN(x) ) ) then  
                                    let cArray  = m.Column(i).ToArray()
                                    let randomS = getNormalSample (cArray) 
                                    cArray |> Array.iteri ( fun ii x -> if ( System.Double.IsNaN(x) ) then do m.[ii,i] <- randomS |> Seq.head ) )
        
        m

    
    /// Replaces NaNs in matrix by given 'impute from row values' function
    let fillSeriesBy (imputeFromRow: seq<float> -> float ) (m:Matrix<float>) =    
        if ( m |> Matrix.exists (fun x -> System.Double.IsNaN(x) ) ) then                              
            [0..m.RowCount-1] 
            |> Seq.iter ( fun rowI -> let currentRow = m.Row(rowI)
                                      if ( currentRow |> Vector.exists (fun x -> System.Double.IsNaN(x) ) ) then
                                        let imputedValue = imputeFromRow currentRow
                                        currentRow |> Seq.iteri ( fun ii x -> if ( System.Double.IsNaN(x) ) then do m.[rowI,ii] <- imputedValue) )
        
        m


