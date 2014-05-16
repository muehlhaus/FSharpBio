namespace FSharpBio.Statistics.Tests

module Testing = 

    open FSharpBio.Statistics.Testing
    open NUnit.Framework
    open FsUnit

    // Test One-Way-ANOVA
    //  
    /// The following is the same example given in Wikipedia's page for the
    /// F-Test [1]. Suppose one would like to test the effect of three levels
    /// of a fertilizer on plant growth. </para>
    /// 
    /// To achieve this goal, an experimenter has divided a set of 18 plants on
    /// three groups, 6 plants each. Each group has received different levels of
    /// the fertilizer under question.</para>
    ///     
    /// After some months, the experimenter registers the growth for each plant: </para>
    /// 
    
    
    let samples = 
        [
            [  6.;  8.;  4.;  5.;  3.;  4.; ]; // records for the first group
            [  8.; 12.;  9.; 11.;  6.;  8.; ]; // records for the second group
            [ 13.;  9.; 11.;  8.;  7.; 12.; ]; // records for the third group
        ]
   

    [<Test>]
    let ``ANOVA should significant pValue`` () =
         (Anova.oneWayAnova samples).Significance  |> should (equalWithin 0.0005) 0.0023
   

   
   
