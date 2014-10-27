namespace FSharp.CoreX

/// Module that contains implementation of useful F#-specific
/// extension members for generic Matrix and Vector types
[<AutoOpen>]
module CoreX =

    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.LinearAlgebra.Double



    // ########################################################################
    // Define type extension for the generic vector type     
    type MathNet.Numerics.LinearAlgebra.
        Vector<'T when 'T : struct and 'T : (new : unit -> 'T) 
                   and 'T :> System.IEquatable<'T> and 'T :> System.IFormattable 
                   and 'T :> System.ValueType> with
      
      /// Shuffels the input vector (method: Fisher-Yates)
      member this.shuffleFisherYates () = 
        let random = new System.Random()
        for i = this.Count downto 1 do
            // Pick random element to swap.
            let j = random.Next(i) // 0 <= j <= i-1
            // Swap.
            let tmp = this.[j]
            this.[j] <- this.[i - 1]
            this.[i - 1] <- tmp
     
        this

    
    
    // ########################################################################
    // Define type extension for the generic matrix type
    type MathNet.Numerics.LinearAlgebra.
        Matrix<'T when 'T : struct and 'T : (new : unit -> 'T) 
                   and 'T :> System.IEquatable<'T> and 'T :> System.IFormattable 
                   and 'T :> System.ValueType> with

      /// Shuffels each column of the input matrix separately  (method: Fisher-Yates)
      member this.shuffleColumnWise () = 
        let random = new System.Random()
        for ci = this.ColumnCount - 1 downto 0 do 
            for ri = this.RowCount downto  1 do
                // Pick random element to swap.
                let rj = random.Next(ri) // 0 <= j <= i-1
                // Swap.
                let tmp = this.[rj,ci]
                this.[rj,ci] <- this.[ri - 1,ci]
                this.[ri - 1,ci] <- tmp
        this


      /// Shuffels each row of the input matrix separately  (method: Fisher-Yates)
      member this.shuffleRowWise () = 
        let random = new System.Random()
        for ri = this.RowCount - 1 downto  0 do
            for ci = this.ColumnCount downto 1 do 
                // Pick random element to swap.
                let cj = random.Next(ci) // 0 <= j <= i-1
                // Swap.
                let tmp = this.[ri,cj]
                this.[ri,cj] <- this.[ri,ci - 1]
                this.[ri,ci - 1] <- tmp
        this


      /// Shuffels each column of the input matrix separately  (method: Fisher-Yates)
      member this.shuffle () = 
        let random = new System.Random()
        for ri = this.RowCount downto 1 do
            for ci = this.ColumnCount downto 1 do 
                // Pick random element to swap.
                let rj = random.Next(ri) // 0 <= j <= i-1
                let cj = random.Next(ci)
                // Swap.
                let tmp = this.[rj,cj]
                this.[rj,cj] <- this.[ri - 1,ci - 1]
                this.[ri - 1,ci - 1] <- tmp
        this




        
    // ########################################################################
    // Define type extension for the double matrix type
    type MathNet.Numerics.LinearAlgebra.Double.Matrix with
        
//        /// Centers the matrix by substracting the column mean 
//        member this.Center () =
//            let colMeans = Matrix.columnMean this |> Seq.toArray            
//            for ci = 0 to this.ColumnCount - 1 do
//                for ri = 0 to this.RowCount - 1 do
//                    this.[ri,ci] <- this.[ri,ci] - colMeans.[ci]
//            this
//
//
//        /// Standardize the matrix by deviding the column standard deviation 
//        member this.Standardize () =
//            let colStDev = [| for (i,coli) in this.ColumnEnumerator() do
//                                yield Fsharp.Stats.DescriptiveStats.stDevPopulation coli |]                        
//            for ci = 0 to this.ColumnCount - 1 do
//                for ri = 0 to this.RowCount - 1 do
//                    if colStDev.[ci] = 0. then raise (System.ArgumentException(sprintf "Standard deviation cannot be zero (cannot standardize the constant variable at column index %i" ci))
//                    this.[ri,ci] <- this.[ri,ci] / colStDev.[ci]
//            this


        /// Returns an System.Collections.Generic.IEnumerable that enumerates over the rows        
        [<System.Obsolete("Do not use. Use EnumerateRows instead.")>]
        member this.toSeqRowWise () =
            this.EnumerateRows()
            |> Seq.map (fun (row) -> row :> seq<float>)


        /// Returns an System.Collections.Generic.IEnumerable that enumerates over the column
        [<System.Obsolete("Do not use. Use EnumerateColumns instead.")>]
        member this.toSeqColumnWise () =
            this.EnumerateColumns()
            |> Seq.map (fun (column) -> column :> seq<float>)


        /// Returns an jagged array over the rows        
        member this.toJaggedArrayRowWise () =
            this.EnumerateRows()
            |> Seq.map (fun (row) -> row  |> Seq.toArray)
            |> Seq.toArray


        /// Returns an an jagged array  over the column
        member this.toJaggedArrayColumnWise () =
            this.EnumerateColumns()
            |> Seq.map (fun (column) -> column |> Seq.toArray)
            |> Seq.toArray


[<AutoOpen>]
module Matrix = 
    
    /// Returns an System.Collections.Generic.IEnumerable that enumerates over the rows  
    let toSeqRowWise (matrix:MathNet.Numerics.LinearAlgebra.Matrix<'T>) =
        matrix.EnumerateRows()
            

    /// Returns an System.Collections.Generic.IEnumerable that enumerates over the column
    let toSeqColumnWise (matrix:MathNet.Numerics.LinearAlgebra.Matrix<'T>) =
        matrix.EnumerateColumns()  
        
    /// Returns an jagged array over the rows  
    let toJaggedArrayRowWise (matrix:MathNet.Numerics.LinearAlgebra.Matrix<'T>) =
        matrix.EnumerateRows()
        |> Seq.map (fun (row) -> row.ToArray())            
        |> Seq.toArray


    /// Returns an an jagged array  over the column
    let toJaggedArrayColumnWise (matrix:MathNet.Numerics.LinearAlgebra.Matrix<'T>) =
        matrix.EnumerateColumns()
        |> Seq.map (fun (column) -> column.ToArray())
        |> Seq.toArray

    //  http://stattrek.com/matrix-algebra/covariance-matrix.aspx
    /// Calculates the variance-covariance matrix of the columns
    let cov (A:MathNet.Numerics.LinearAlgebra.Matrix<float>)=     
        let n = float A.RowCount
        // 11'
        let onesMatrix = MathNet.Numerics.LinearAlgebra.Double.DenseMatrix.Create(A.RowCount, A.RowCount, 1.)    
        // deviation scores matrix  a = A - 11'A (1/n)
        let a = A - onesMatrix * A * (1. / n)
        // deviation sum of squares matrix
        let a'a = a.Transpose() * a
        a'a.Divide n

    
    /// Concatenates two matrices along the column
    let append (left:MathNet.Numerics.LinearAlgebra.Matrix<'T>) (right:MathNet.Numerics.LinearAlgebra.Matrix<'T>) =
            left.Append(right)

    /// Stacks two matrices along the rows        
    let stack (upper:MathNet.Numerics.LinearAlgebra.Matrix<'T>) (lower:MathNet.Numerics.LinearAlgebra.Matrix<'T>) =
            upper.Stack(lower)
