namespace FSharpBio.Statistics.Fitting

module Bootstrap =
    
    // When we sample with replacement, the two sample values are independent.
    // Practically, this means that what we get on the first one doesn't affect what we get on the second.
    // Mathematically, this means that the covariance between the two is zero
    /// Samples from an array of obj wit replacement (with putting back)
    let sampleWithReplacement (data:array<_>) (k:int) =
        let m = if (Seq.length(data)) < k then (Seq.length(data)) else k
        let rnd = new System.Random()
        Array.init m (fun i -> data.[rnd.Next(0,m - 1)])


    // Implementation according to: http://krkadev.blogspot.de/2010/08/random-numbers-without-repetition.html
    /// Samples from an array of obj without replacement (without putting back)
    let sampleWithOutReplacement (source:array<_>) (k:int) =
        let n = source.Length
        let random = new System.Random()
        let used = new System.Collections.Generic.Dictionary<int,int>()
        // recursive do-while implementation
        let rec loop (index:int) (off:int) (n:int) (i:int) =    
            let value = n - i - 1 
            let redirect = if used.ContainsValue(value) then
                            Some(off)
                           else
                            if used.ContainsKey(off) then used.[off] <- value else used.Add(off,value)
                            None
        
            if redirect.IsSome then   (*While*)
                loop (index + 1) (redirect.Value) (n) (i)
            else
                off
        Array.init k (fun i -> source.[(loop 0 (random.Next(n - i)) n i)] )

