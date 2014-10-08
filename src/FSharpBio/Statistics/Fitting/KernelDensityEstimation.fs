namespace FSharpBio.Statistics.Fitting


/// kernel density estimates
module KernelDensityEstimation =
    
    open FSharp.CoreX     
    open MathNet.Numerics.IntegralTransforms
    
    /// Different Kernel types to use in KernelDE.Density
    type KernelTyps =   Gaussian
                        | Rectangular
                        | Triangular 
                        | Epanechnikov 
                        | Biweight
                        | Cosine
                        | Optcosine

    
    // 
    let private massdist (_x:float[]) (_xmass:float[]) (_nx:int) (_xlow:float) (_xhigh:float) (_y:int) (_ny:int) =

    //    double fx, xdelta, xmi, xpos;   /* AB */
    //    int i, ix, ixmax, ixmin;

        let ixmin = 0;
        let ixmax = _ny - 2
        //* AB: line deleted */
        let xdelta = (_xhigh - _xlow) / (float(_ny - 1))
    
        let y = Array.init _y (fun yv -> 0.0)
    
        for i in [0.._nx-1] do
            let xpos = (_x.[i] - _xlow) / xdelta
            let ix = int(floor(xpos))
            let fx = xpos - float(ix)
            let xmi = _xmass.[i];   //* AB: new line  */
    
            if (ixmin <= ix && ix <= ixmax) then
                y.[ix] <- y.[ix] + (1. - fx) * xmi    //* AB */
                y.[ix + 1] <- y.[ix + 1] + fx * xmi      //* AB */    
            elif (ix = -1) then
                y.[0]  <- y.[0] + fx * xmi    
            elif (ix = ixmax + 1) then
                y.[ix] <- y.[ix] + (1. - fx) * xmi
    
        y


    // #####
    // The algorithm used in density.default disperses the mass of the empirical distribution function over
    // a regular grid of at least 512 points and then uses the fast Fourier transform to convolve this approximation 
    // with a discretized version of the kernel and then uses linear approximation to evaluate the density at the specified points.
    // 
    // Infinite values in x are assumed to correspond to a point mass at +/-Inf and the density estimate is of the sub-density on (-Inf, +Inf).

    /// Computes kernel density estimates. Its default method does so with the given kernel and bandwidth for univariate observations.
    type Density
        ( data          : seq<float>,
          ?bandwidth    : float,
          ?kernel       : KernelTyps,
          ?adjust       : float,
          ?weights      : float[],
          ?width        : float,
          ?n            : int,
          ?from         : float,
          ?tto          : float,
          ?cut          : int
        ) = class
           
        //Handel x data
        let x = data |> Seq.Double.filterNaN |> Seq.Double.filterInfinity |> Seq.toArray    
        let N = Seq.length(data)
        let nx = x.Length    
        //Handel Grid and Kernel
        let _n      = let tmp = defaultArg n 512
                      if (tmp > 512) then int(2.**ceil(log(float(tmp)))) else 512
        let _kernel = defaultArg kernel KernelTyps.Gaussian    
        //Handle 'weights'    
        let mutable _totalMass = 1.
        let _weights = if weights.IsNone then
                        (Seq.initInfinite ( fun v -> 1./float(nx)) |> Seq.take (nx) |> Seq.toArray)
                       else
                        if (weights.Value.Length <> N) then
                            raise (System.ArgumentException("'x' and 'weights' have unequal length"))
                        elif (weights.Value |> Array.exists ( fun x -> (infinity.Equals(x)) ) ) then
                            raise (System.ArgumentException("'weights' must all be finite"))
                        elif (weights.Value |> Array.exists ( fun x -> x < 0. ) ) then
                            raise (System.ArgumentException("'weights' must not be negative"))
                        else
                            let wsum = Array.sum(weights.Value)
                            if wsum <> 1. then printfn ("sum(weights) != 1  -- will not get true density but 'sub-density'")
                            if (nx <> N) then
                                let xfWeights = Seq.map2 ( fun d w -> (d,w) ) data weights.Value 
                                                |> Seq.filter ( fun (d,w) -> not(nan.Equals(d) && nan.Equals(w)) || not(infinity.Equals(d) && infinity.Equals(w)) )
                                                |> Seq.map ( fun (d,w) -> w ) |> Seq.toArray
                                _totalMass <- Array.sum(xfWeights) / wsum
                                xfWeights
                            else
                                weights.Value
                    
                            
        //Handel bandwidth
        let _adjust = defaultArg adjust 1.
        let _bandwidth = let tmpBW =
                             if ( bandwidth.IsNone && width.IsSome ) then
                                match _kernel with
                                | Gaussian       -> ( width.Value / 4. )
                                | Rectangular    -> ( width.Value / (2.*sqrt(3.)) )
                                | Triangular     -> ( width.Value / (2.*sqrt(3.)) )
                                | Epanechnikov   -> ( width.Value / (2.*sqrt(3.)) )
                                | Biweight       -> ( width.Value / (2.*sqrt(3.)) )
                                | Cosine         -> ( width.Value / (2.*sqrt(3.)) )
                                | Optcosine      -> ( width.Value / (2.*sqrt(3.)) )
                             else
                                if bandwidth.IsSome then
                                    bandwidth.Value
                                else
                                    if (nx < 2 ) then raise (System.ArgumentException("need at least 2 points to select a bandwidth automatically"))
                                    Bandwidth.nrd0 (x)
                            
                         let bw = tmpBW * _adjust
                         if (bw <= 0.0) then raise (System.ArgumentException("'bandwidth' is not positive."))
                         if (infinity.Equals(bw)) then raise (System.ArgumentException("non-finite 'bandwidth'."))
                         bw

        //Handel y data
        let _cut    = defaultArg cut 3
        let _from   = defaultArg from (Array.min(x) - float(_cut) * _bandwidth)
        let _to     = defaultArg tto  (Array.max(x) + float(_cut) * _bandwidth)
        let _lo = _from - 4. * _bandwidth
        let _up = _to + 4. * _bandwidth
        let _y = (massdist x _weights nx _lo (_up) ((2*_n)) (_n)) |> Array.map ( fun yValue -> yValue * _totalMass)
        //Handel Kernel function
        let kkernelFunction = 
            let norm = MathNet.Numerics.Distributions.Normal(0.,_bandwidth)
            match _kernel with
            | Gaussian       -> (fun x ->   norm.Density(x))
            | Rectangular    -> (fun x ->   let a = _bandwidth * sqrt(3.)
                                            if abs(x)  < a then ( 0.5 / a ) else 0. )
            | Triangular     -> (fun x ->   let a = _bandwidth*sqrt(6.)
                                            let ax = abs(x)
                                            if ax < a then ( 1. - ax / a ) else 0. )
            | Epanechnikov   -> (fun x ->   let a = _bandwidth*sqrt(5.)
                                            let ax = abs(x)
                                            if ax < a then ( 3. / 4. * (1. - (ax / a)**2.) / a ) else 0. )
            | Biweight       -> (fun x ->   let a = _bandwidth*sqrt(7.)
                                            let ax = abs(x)
                                            if ax < a then ( 15. / 16. * (1. - (ax / a)**2.)**2. / a ) else 0. )
            | Cosine         -> (fun x ->   let a = _bandwidth / sqrt(1./3. - 2./ System.Math.PI **2.)
                                            if abs(x)  < a then ( (1. + cos(System.Math.PI*x/a)) / 2. * a ) else 0. )
            | Optcosine      -> (fun x ->   let a = _bandwidth / sqrt(1. - 8. / System.Math.PI **2.)
                                            if x  < a then ( System.Math.PI / 4. * cos(System.Math.PI*x/(2. * a)) / a ) else 0. )

        let kords =
            let norm = MathNet.Numerics.Distributions.Normal(0.,_bandwidth)
            //let fft = new MathNet.Numerics.IntegralTransforms.Algorithms.DiscreteFourierTransform()            
            let tmp =  let a = Seq.Double.seqInit (0.0) (2.*(_up-_lo)) (float(2 * _n)) |> Seq.toArray
                       (a.[(_n + 1)..(2 * _n)-1] <- (a.[1.._n-1] |> Array.rev |> Array.map (fun x -> x * -1.0)  ) )
                       a |> Array.map (fun x -> kkernelFunction(x))
            let fftY     = Fourier.NaiveForward ((_y |> Array.map  ( fun d -> System.Numerics.Complex(d,0.) ) ), MathNet.Numerics.IntegralTransforms.FourierOptions.NoScaling)
            let fftKords = Fourier.NaiveForward ((tmp|> Array.map  ( fun d -> System.Numerics.Complex(d,0.))), MathNet.Numerics.IntegralTransforms.FourierOptions.NoScaling)
            Fourier.NaiveInverse ( (Array.map2 ( fun y k -> y * System.Numerics.Complex.Conjugate(k) ) fftY fftKords),MathNet.Numerics.IntegralTransforms.FourierOptions.NoScaling)
            |> Array.toSeq
            |> Seq.truncate (_n)    
            |> Seq.map ( fun c -> let tmp = c.Real / float(_y.Length)
                                  if tmp > 0. then tmp else 0.)

    //        |> Seq.map ( fun x -> let tmp = x / float(_y.Length)
    //                              if tmp > 0. then tmp else 0.)
            //approx
        let xords = Seq.Double.seqInit _lo _up (float(_n))   //Array.init (_n) ( fun x -> (float(x)+_lo) / float(_n) * _up)
        let nx    = Seq.Double.seqInit _from _to (float(_n))    |> Seq.toArray//Array.init (_n) ( fun x -> (float(x)+_from) / float(_n) * _to)
        let kdeY = Approximation.approx xords kords nx Seq.average |> Seq.toArray
        
        let sumY = kdeY |> Array.sum

        // Read-only properties
        member this.X           = nx           
        member this.Y           = kdeY
        member this.Kords       = kords
        member this.BandWidth   = _bandwidth     
        member this.KernelType  = _kernel
        member this.Weights     = _weights
        member this.From        = _from
        member this.To          = _to
        member this.Cut         = _cut

        member this.ApproxVector(xv:float[]) =
            Approximation.approx xords kords xv Seq.average |> Seq.toArray
        
        /// <summary>
        ///   Gets the cumulative distribution function (cdf) for
        ///   this distribution evaluated at point <c>x</c>.
        /// </summary>
        /// 
        /// <param name="x">A single point in the distribution range.</param>
        /// 
        /// <remarks>
        ///   The Cumulative Distribution Function (CDF) describes the cumulative
        ///   probability that a given value or any value smaller than it will occur.
        /// </remarks>
        /// 
        member this.DistributionFunction(d:float) =
            let fx = Array.zip (nx) (kdeY) |> Array.filter (fun (nx,ny) -> nx <= d) |> Array.sumBy (fun (nx,ny) -> ny)            
            fx / sumY 

        member this.PDF(d:float) =
            let fx = Array.zip (nx) (kdeY) |> Array.filter (fun (nx,ny) -> nx <= d) 
            snd(fx.[fx.Length-1]) // / sumY
//            let p = kdeY |> Array.fold (fun si state -> let z = d - si 
//                                                        state + exp(-z * z * 0.5)                                                 
//                                        ) 0.0
//            let p' = p * 1.0 / ( 2.50662827463100050242E0 )
//            p'/ float(kdeY.Length)
        
        
        member this.EmpiricalDistributionFunction(d:float) =
            let fx = x |> Array.filter ( fun v -> v <= d) 
            float(fx.Length) / float(x.Length)


    end




