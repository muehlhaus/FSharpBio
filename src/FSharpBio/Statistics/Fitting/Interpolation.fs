namespace FSharpBio.Statistics.Fitting

module InterPolation = 


    // Attention filtered list must not be shorter then 5 items
    let akimaSpline a =
        let samples = a |> Array.filter(fun x -> not (System.Double.IsNaN(x)))
        let achses = 
            a
            |> Array.mapi (fun i x -> if(System.Double.IsNaN(x)) then nan else float(i))
            |> Array.filter(fun x -> not (System.Double.IsNaN(x)))
        let interPol = MathNet.Numerics.Interpolation.Algorithms.AkimaSplineInterpolation(achses,samples)
        a
        |> Array.mapi (fun i x -> if(System.Double.IsNaN(x)) then interPol.Interpolate(float(i)) else x)

    /// creates a Floater-Hormann barycentric interpolation (rational interpolation without poles)
    let rationalWithoutPoles a =
        let samples = a |> Array.filter(fun x -> not (System.Double.IsNaN(x)))
        let achses = 
            a
            |> Array.mapi (fun i x -> if(System.Double.IsNaN(x)) then nan else float(i))
            |> Array.filter(fun x -> not (System.Double.IsNaN(x)))
        let interPol = MathNet.Numerics.Interpolation.Algorithms.FloaterHormannRationalInterpolation(achses,samples)
        a
        |> Array.mapi (fun i x -> if(System.Double.IsNaN(x)) then interPol.Interpolate(float(i)) else x)
        
    
    let linearSpline a =
        let samples = a |> Array.filter(fun x -> not (System.Double.IsNaN(x)))
        let achses = 
            a
            |> Array.mapi (fun i x -> if(System.Double.IsNaN(x)) then nan else float(i))
            |> Array.filter(fun x -> not (System.Double.IsNaN(x)))
        let interPol = MathNet.Numerics.Interpolation.Algorithms.LinearSplineInterpolation(achses,samples)
        a
        |> Array.mapi (fun i x -> if(System.Double.IsNaN(x)) then interPol.Interpolate(float(i)) else x)


    
    /// Hermite spline
    //The first step in the algorithm is to compute the tangents using three-point differences:
    let hermite_tangents (x:float[]) (y:float[]) =
      let n = Array.length x
      if not ((Array.length y) = n) then raise (System.Exception("hermite_tangents")) else
      let m = Array.zeroCreate n 
      let mutable d = ((y.[1] - y.[0]) / (x.[1] - x.[0]))
      m.[0] <- d
      for i = 1 to n - 2 do
        let d' = (y.[i+1] - y.[i]) / (x.[i+1] - x.[i])
        m.[i] <- 0.5 * (d + d')
        d <- d'
      m.[n - 1] <- d
      m


    let monotone_hermite_tangents x y =
      let n = Array.length x
      let m = hermite_tangents x y
      for i = 0 to n - 2 do
        let d = (y.[i+1] - y.[i]) / (x.[i+1] - x.[i])
        if d = 0. then
          m.[i] <- 0.
          m.[i+1] <- 0.
        else
          let a = m.[i  ] / d
          let b = m.[i+1] / d
          let s = a * a + b * b
          if s > 9. then
            let t = 3. * d / (sqrt s)
            m.[i  ] <- t * a
            m.[i+1] <- t * b
      m

    //http://en.wikipedia.org/wiki/Binary_search_algorithm
    let private bin_search e x =
      let n = Array.length x
      let mutable l = 0
      let mutable h = n
      while l + 1 <> h do
        let m = (l + h) / 2
        if e < x.[m] then h <- m else l <- m
      l


    let hermite_interpolation (x:float[]) (y:float[]) (m:float[]) e =
      let n = Array.length x
      if e <= x.[0]   then y.[0] else
      if e >= x.[n-1] then y.[n-1] else
      let i = bin_search e x
      let l = x.[i]
      let h = x.[i+1] - l
      let t = (e - l) / h
      let u = t * t
      let h00 =  ( 2. * t - 3.) * u + 1.
      let h10 = ((      t - 2.) * t + 1.) * t
      let h01 =  (-2. * t + 3.) * u
      let h11 =  (      t - 1.) * u
      y.[i] * h00 + y.[i+1] * h01 + h * (m.[i] * h10 + m.[i+1] * h11) 



