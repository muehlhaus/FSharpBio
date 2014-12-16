namespace FSharpBio.Statistics.Testing

/// <summary>
///   Two-Sample (Goodness-of-fit) Chi-Square Test (Upper Tail)
/// </summary>
/// 
/// <remarks>
/// <para>
///   A chi-square test (also chi-squared or χ2  test) is any statistical
///   hypothesis test in which the sampling distribution of the test statistic
///   is a <see cref="ChiSquareDistribution">chi-square distribution</see> when
///   the null hypothesis is true, or any in which this is asymptotically true,
///   meaning that the sampling distribution (if the null hypothesis is true) 
///   can be made to approximate a chi-square distribution as closely as desired
///   by making the sample size large enough.</para>
/// <para>
///   The chi-square test is used whenever one would like to test whether the
///   actual data differs from a random distribution. </para>
///   
/// <para>
///   References:
///   <list type="bullet">
///     <item><description><a href="http://en.wikipedia.org/wiki/Chi-square_test">
///        Wikipedia, The Free Encyclopedia. Chi-Square Test. Available on:
///        http://en.wikipedia.org/wiki/Chi-square_test </a></description></item>
///   
///     <item><description><a href="http://www2.lv.psu.edu/jxm57/irp/chisquar.html">
///        J. S. McLaughlin. Chi-Square Test. Available on:
///        http://www2.lv.psu.edu/jxm57/irp/chisquar.html </a></description></item>
///   </list></para>
/// </remarks>
/// 
module ChiSquareTest =

    open FSharpBio.Statistics.Descriptive
    open FSharpBio.Statistics.Testing.TestStatistics


    /// <summary>
    ///   Computes the Chi-Square test statistics for a given statistic
    ///   with given degrees of freedom.
    /// </summary>
    /// 
    /// <param name="statistic">The test statistic.</param>
    /// <param name="df">The degrees of freedom for the numerator.</param>    
    type ChiSquare
        (   statistic            : float,
            df                   : float          
        ) = class

        let ChiStat = MathNet.Numerics.Distributions.ChiSquared(df)
        let cdf     = ChiStat.CumulativeDistribution(statistic) 
        let pvalue  = if statistic > 0. then 1.-cdf else cdf

        member this.DegreesOfFreedom      = df        
        member this.StatisticDistribution = ChiStat
        member this.Statistic             = statistic
        /// One Tailed/Sided
        member this.PValue_Left           = 1. - pvalue
        /// One Tailed/Sided
        member this.PValue_Right          = pvalue
        /// Two Tailed/Sided
        member this.PValue                = pvalue * 2.
    
    end


    /// Computes the Chi-Square test
    // n data points -> degrees of freedom = n - 1
    let compute (degreesOfFreedom:int) (expected:seq<float>) (observed:seq<float>) =
        let chi2 =
            Seq.zip observed expected
            |> Seq.fold (fun acc (obs,exp) -> let d = obs - exp
                                              acc + (d * d) / exp) 0.0
        ChiSquare(chi2,float degreesOfFreedom)
        

    /// Bartlett's test for equality of variances
    /// Tests the null hypothesis that all group variances are equal
    let bartlettTest (samples : seq<#seq<float>>) =
        let sizes        = samples |> Seq.map Seq.length        
        let k            = Seq.length samples |> float
        let popVariances = samples |> Seq.map StatisticalMeasure.varPopulation
        let Sp           =  StatisticalMeasure.UtilityFunctions.pooledVarPopulationOf sizes popVariances
        let numeratorSum,N = 
            Seq.zip popVariances sizes
            |> Seq.fold (fun (varAcc,nAcc) (variance,size) -> let n      = float (size)
                                                              let logVar = log (variance * (n - 1.))
                                                              (varAcc + logVar,nAcc + n)) (0., 0.) 
        let denominatorSum = sizes |> Seq.sumBy (fun n -> 1. / float (n - 1))
        
        let num = (N - k) * log(Sp) - numeratorSum
        let den = 1. + (1. / (3.0 * (k - 1.))) * (denominatorSum - 1.0 / (N - k))

        let W = num / den
        let df = k - 1.
        
        ChiSquare(W, df)



//    let leveneTest (samples : seq<#seq<float>>) = 
//        {
//            int N = 0, k = samples.Length;
//
//            // Compute group means
//            var means = new double[samples.Length];
//            if (median)
//                for (int i = 0; i < means.Length; i++)
//                    means[i] = Accord.Statistics.Tools.Median(samples[i]);
//            else
//                for (int i = 0; i < means.Length; i++)
//                    means[i] = Accord.Statistics.Tools.Mean(samples[i]);
//
//            // Compute absolute centred samples
//            var z = new double[samples.Length][];
//            for (int i = 0; i < z.Length; i++)
//            {
//                z[i] = new double[samples[i].Length];
//                for (int j = 0; j < z[i].Length; j++)
//                    z[i][j] = Math.Abs(samples[i][j] - means[i]);
//            }
//
//            // Compute means for the centred samples
//            var newMeans = new double[samples.Length];
//            for (int i = 0; i < newMeans.Length; i++)
//                newMeans[i] = Accord.Statistics.Tools.Mean(z[i]);
//
//            // Compute total mean
//            double totalMean = 0;
//            for (int i = 0; i < samples.Length; i++)
//            {
//                for (int j = 0; j < samples[i].Length; j++)
//                    totalMean += z[i][j];
//                N += samples[i].Length;
//            }
//            totalMean /= N;
//
//            double sum1 = 0; // Numerator sum
//            for (int i = 0; i < samples.Length; i++)
//            {
//                int n = samples[i].Length;
//                double u = (newMeans[i] - totalMean);
//                sum1 += n * u * u;
//            }
//
//            double sum2 = 0; // Denominator sum
//            for (int i = 0; i < samples.Length; i++)
//            {
//                for (int j = 0; j < samples[i].Length; j++)
//                {
//                    double u = z[i][j] - newMeans[i];
//                    sum2 += u * u;
//                }
//            }
//
//            double num = (N - k) * sum1;
//            double den = (k - 1) * sum2;
//
//            double W = num / den;
//            int degree1 = k - 1;
//            int degree2 = N - k;

