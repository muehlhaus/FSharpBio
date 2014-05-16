namespace FSharpBio.Statistics.Testing

/// [omit]
module TestStatistics =
    

    /// <summary>
    ///   Creates a new T-Test for a given statistic
    ///   with given degrees of freedom.
    /// </summary>
    /// 
    /// <param name="statistic">The test statistic.</param>
    /// <param name="df">The degrees of freedom for the numerator.</param>    
    type TTEST
        (   statistic            : float,
            df                   : float          
        ) = class

        let TStat  = MathNet.Numerics.Distributions.StudentT(0.,1.,df)
        let cdf    = TStat.CumulativeDistribution(statistic) 
        let pvalue = if statistic > 0. then 1.-cdf else cdf

        member this.DegreesOfFreedom      = df        
        member this.StatisticDistribution = TStat
        member this.Statistic             = statistic
        /// One Tailed/Sided
        member this.PValue_Left           = 1. - pvalue
        /// One Tailed/Sided
        member this.PValue_Right          = pvalue
        /// Two Tailed/Sided
        member this.PValue                = pvalue * 2.
    
    end


    /// <summary>
    ///   Creates a new F-Test for a given statistic
    ///   with given degrees of freedom.
    /// </summary>
    /// 
    /// <param name="statistic">The test statistic.</param>
    /// <param name="d1">The degrees of freedom for the numerator.</param>
    /// <param name="d2">The degrees of freedom for the denominator.</param>
    type FTEST
        ( statistic            : float,
          d1                   : float,
          d2                   : float
        ) = class

        let FStat  = MathNet.Numerics.Distributions.FisherSnedecor(d1,d2)
        let pvalue = 1.0 - FStat.CumulativeDistribution(statistic)

        member this.DegreesOfFreedom1     = d1
        member this.DegreesOfFreedom2     = d2
        member this.StatisticDistribution = FStat
        member this.Statistic             = FStat.CumulativeDistribution(statistic)
        member this.PValue                = pvalue
    
    end


    // ###########################################################################################################
    // the hypergeometric distribution is a discrete probability distribution that describes the probability of 
    //   k successes in
    //   n draws from a finite 
    //   x population of size containing
    //   m successes without replacement (successes states)
    /// Calculates p value based on hypergeometric distribution (p value <= k)
    type HypergeometricTEST
        ( sampleSuccess        : int,
          sampleSize           : int,          
          populationSuccess    : int,
          populationSize       : int                 
        ) = class

        let HyperStat  = MathNet.Numerics.Distributions.Hypergeometric(populationSize,populationSuccess,sampleSize)
        let pvalue     = 1.0 - HyperStat.CumulativeDistribution(float sampleSuccess +  1.)

        member this.DegreesOfFreedom1     = populationSize
        member this.DegreesOfFreedom2     = sampleSize
        member this.StatisticDistribution = HyperStat
        member this.Statistic             = HyperStat.CumulativeDistribution(float sampleSuccess +  1.)
        member this.PValue                = pvalue
    
    end        






      
        

