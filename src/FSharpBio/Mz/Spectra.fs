namespace FSharpBio.Mz


module Spectra =
    
    // ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
    /// Fragment ion types  
    type IonTypes =
    | Unknown
    | Precursor
    | A         
    | B         
    | C         
    | X         
    | Y         
    | Z         
    | AlossH2O  
    | AlossNH3  
    | BlossH2O  
    | BlossNH3  
    | ClossH2O  
    | ClossNH3  
    | XlossH2O  
    | XlossNH3  
    | YlossH2O  
    | YlossNH3  
    | ZlossH2O  
    | ZlossNH3  
    | Immonium 


    // ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
    /// Single peak 
    type Peak = { IonType   : IonTypes;
                  Mass      : float;
                  Intensity : float;
                  P         : float; //percent of total
                }

    /// Creates a peak
    let createPeak ionType mass intensity percentOfTotalIntensity =
        { IonType = ionType; Mass = mass; Intensity = intensity; P = percentOfTotalIntensity }
    

    // ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
    /// Type abbreviation for seq<Peak>  
    type Spectrum = seq<Peak>

    let createSpectrum (mass:seq<float>) (intensity:seq<float>) (totalIntensity:float) =
        //let totalIntensity = intensity |> Seq.sum
        Seq.map2 (fun m i -> createPeak IonTypes.Unknown m i (i/totalIntensity)) mass intensity

    
    let createSpectrumMassIntensity (massIntensity:seq<float*float>) (totalIntensity:float) =
        //let totalIntensity = intensity |> Seq.sum
        Seq.map (fun (m,i) -> createPeak IonTypes.Unknown m i (i/totalIntensity)) massIntensity

    /// Creates a spectrum of mass sequence and intensity sequence
    let spectrumOfMassAndIntensity (mass:seq<float>) (intensity:seq<float>) =
        let totalIntensity = intensity |> Seq.sum
        Seq.map2 (fun m i -> createPeak IonTypes.Unknown m i (i/totalIntensity)) mass intensity    

//    /// Creates a spectrum of mass sequence and intensity sequence
//    let spectrumOfMassIntensity (massIntensity:seq<float*float>) =
//        let totalIntensity = intensity |> Seq.sum
//        Seq.map2 (fun m i -> createPeak IonTypes.Unknown m i (i/totalIntensity)) mass intensity



//    let createSpectrumWithIonTypes (ionTypes:seq<IonTypes>) (mass:float[]) (intensity:float[]) =
//        let totalIntensity = intensity |> Array.sum
//        Seq.map2 (fun m i -> createPeak IonTypes.Unknown m i (i/totalIntensity)) mass intensity       

    type PrecursorInfo = {IsolationWindowTargetMz:float; IsolationWindowLowerOffset:float; IsolationWindowUpperOffset:float; DissociationMethod:string; ChargeState:int;}

    type SpectrumHeader = {RawFile:string; SpectrumID:string; SpectrumRepresentation:string; MSLevel:int; ScanPolarity:string; RetentionTime:float; PrecursorInfo:PrecursorInfo;}

    // ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
    /// Scan includes scan header and spectrum
    type Scan = { 
                    Spectrum : Spectrum;
                    Header : SpectrumHeader;                                                          
                }

    let createScan spectrum spectrumHeader =
        { Spectrum = spectrum; Header = spectrumHeader; }
    

    

