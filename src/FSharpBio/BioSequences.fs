namespace FSharpBio

/// Module for basic function for type BioSeq
module BioSequences = 
    
    open System.Collections
    open System.Collections.Generic

//    ///http://stackoverflow.com/questions/8438244/implementing-ienumerablet-when-base-class-implements-ienumerable
//    let mkEnum<'T>(ie : System.Collections.IEnumerable) 
//                           : System.Collections.Generic.IEnumerator<'T> =
//        (seq { for x in ie -> unbox x }).GetEnumerator()
    
    /// Type for sequence of BioItem, Id, DisplayId and a MetaData object
    //[<CustomEquality; CustomComparison>]
    type BioSeq<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IBioItem> 
        ( sequence  : seq<'a>,
          id        : string,
          displayId : string,
          metadata  : obj
        ) = 
        member internal this.asSeq = sequence
        member this.Id             = id
        member this.DisplayId      = displayId
        
        member this.MetaData       = metadata

        // 
        override this.Equals(other) =
            match other with
            | :? BioSeq<'a> as o -> (Seq.compareWith Unchecked.compare this.asSeq o.asSeq) = 0
            | _ -> false
 
        override this.GetHashCode() = hash this.asSeq
        interface System.IComparable with
          member this.CompareTo other =
              match other with
              | :? BioSeq<'a> as o -> Seq.compareWith Unchecked.compare this.asSeq o.asSeq 
              | _ -> invalidArg  "other" "cannot compare values of different types"
        
        interface IEnumerable<'a> with
                member this.GetEnumerator() = this.asSeq.GetEnumerator()
 
        interface IEnumerable with
                member this.GetEnumerator() =  this.asSeq.GetEnumerator() :> IEnumerator

        // seqequence only constructor
        new (sequence) = 
         BioSeq(sequence,"","",null)
         
        // seqequence only constructor
        new (sequence,id) = 
         BioSeq(sequence,id,"",null)
         

    /// Signiture type for sequence of amino acids
    type AminoAcidSeq<'a when 'a :> AminoAcids.IAminoAcid>    = BioSeq<'a>

    /// Signiture type for sequence of nucleotides
    type NucleotidSeq<'a when 'a :> Nucleotides.INucleotides> = BioSeq<'a>

    
    /// Generates amino acid sequence of one-letter-code string
    let ofAminoAcidString (converter:bioItemOptionConverter<AminoAcids.AminoAcid>) (s:string) =          
        let aas = s |> Seq.choose converter
        BioSeq aas

    /// Generates amino acid sequence of one-letter-code string
    let ofAminoAcidStringWithId (converter:bioItemOptionConverter<AminoAcids.AminoAcid>) (id,displayId) (s:string) =          
        let aas = s |> Seq.choose converter
        BioSeq(aas,id,displayId,null)

    

    /// Generates nucleotide sequence of one-letter-code string
    let ofNucleotideString (converter:bioItemOptionConverter<Nucleotides.Nucleotide>) (s:string) =             
        let nucs = s |> Seq.choose converter           
        BioSeq nucs

    /// Generates nucleotide sequence of one-letter-code string with Id and DisplayId
    let ofNucleotideStringWithId (converter:bioItemOptionConverter<Nucleotides.Nucleotide>) (id,displayId) (s:string) =             
        let nucs = s |> Seq.choose converter           
        BioSeq(nucs,id,displayId,null)


    /// Returns string of one-letter-code
    let toString (bs:BioSeq<_>) =
        new string [|for c in bs  -> BioItem.symbol c|]         


       
    /// Returns formula
    let toFormula (bs:BioSeq<_>) =
        bs |> Seq.fold (fun acc item -> Formula.add acc  (BioItem.formula item)) Formula.emptyFormula
        


    /// Returns string of one-letter-code with modifications and label
    /// representation equivalent to ABSciex protein pilot software
    let toStringProteinPilot (bs:BioSeq<_>) =
        let getString (b:#IBioItem) =            
            match box b with
            | :? AminoAcids.AminoAcid     as a -> let massDiffInt = AminoAcids.monoisoMassDiff a |> round |> int                                                   
                                                  sprintf "%c[%0+3i]" (AminoAcids.toChar a) massDiffInt
            | :? Nucleotides.INucleotides as n -> sprintf "%c" (BioItem.symbol n)
            | _                                -> failwithf "Type is unknown: %A" b

        new string [|for c in bs  do yield! (getString c)|]  

//    /// Returns average mass of AminoAcidSequence including H20
//    let averageMass (aas:AminoAcidSequence) =
//        Formula.averageMass (toFormula aas)
//                
//
//    /// Returns monoisotopic mass of AminoAcidSequence including H20
//    let monoisoMass (aas:AminoAcidSequence) =
//        Formula.monoisoMass (toFormula aas)

    
//    /// Filters gaps 
//    let filterGAP (aas:AminoAcidSequence) =
//        aas |> Seq.filter (fun a -> a <> AminoAcids.Table.Gap) 




//    let hydrophobicity (aas:AminoAcidSeq<_>) = 
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.hydrophobicity
//
//    let hydropathicity (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.hydropathicity
//
//    let omh (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.omh
//    
//    let bulkiness (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.bulkiness
//
//    let granthamPolarity (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.granthamPolarity
//
//    let recFactors (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.recFactors
//
//    let molecularWeight (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.molecularWeight
//
//    let bullHydrophobicity (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.bullHydrophobicity
//
//    let guyHydrophobicity (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.guyHydrophobicity
//
//    let miyazawaHydrophobicity (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.miyazawaHydrophobicity
//
//    let rosemanHydrophobicity (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.rosemanHydrophobicity
//
//    let hydrationPotential (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.hydrationPotential
//
//    let constantsHydrophob (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.constantsHydrophob
//
//    let indicesHydrophob (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.indicesHydrophob
//
//    let mobility (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.mobility
//
//    let tfaRtCoeff (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.tfaRtCoeff
//
//    let hplcPh2rtCoeff (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.hplcPh2rtCoeff
//
//    let molFraction2001BuriedRes (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.molFraction2001BuriedRes
//
//    let proportionOfRes95Buried (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.proportionOfRes95Buried
//
//    let atomicWeightRatioHeteroElem (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.atomicWeightRatioHeteroElem
//
//    let avgFlexibilityIndex (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.avgFlexibilityIndex
//
//    let betaSheet29 (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.betaSheet29
//
//    let alphaHelix (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.alphaHelix
//
//    let betaTurn (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.betaTurn
//
//    let normFrequencyAlphaHelix (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.normFrequencyAlphaHelix
//
//    let normFrequencyBetaTurn (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.normFrequencyBetaTurn
//
//    let antiparallelBetaConfPreference (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.antiparallelBetaConfPreference
//
//    let overallAaComp (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.overallAaComp
//
//    let relMutability (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.relMutability
//
//    let numberOfCodonsUniversalGenCode (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.numberOfCodonsUniversalGenCode
//
//    let zimmermanPolarity (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.zimmermanPolarity
//
//    let refractivity (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.refractivity
//
//    let normConsensusHydrophob (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.normConsensusHydrophob
//
//    let hydrophilicity (aas:AminoAcidSeq<_>) =
//        aas
//        |> Seq.map AminoAcids.toLiteral
//        |> Seq.map AminoAcidLiteral.Propteries.hydrophilicity
//
//    let avgSorroundingHydrophob (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.avgSorroundingHydrophob
//    
//    let lAlphaHydrophobicity (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.lAlphaHydrophobicity
//    
//    let fauchereHydrophobicity (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.fauchereHydrophobicity
//    
//    let freeEnergyofTransfer (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.freeEnergyofTransfer
//    
//    let membraneBuriedHelixParam (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.membraneBuriedHelixParam
//    
//    let contribHydrophobInteractionsToStability (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.contribHydrophobInteractionsToStability
//    
//    let antigenicity (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.antigenicity
//    
//    let hydrophilicityScaleHPLCRtTime (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.hydrophilicityScaleHPLCRtTime
//    
//    let hydrophobIndex (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.hydrophobIndex
//    
//    let hfbaRtCoeff (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.hfbaRtCoeff
//    
//    let transmembraneTendency (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.transmembraneTendency
//
//    let hplcPH7RtCoeff (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.hplcPH7RtCoeff
//
//    let molFractionAccessibleResidues (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.molFractionAccessibleResidues
//
//    let meanFractionalAreaLoss (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.meanFractionalAreaLoss
//
//    let avgBuriedArea (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.avgBuriedArea
//
//    let alphaHelix29 (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.alphaHelix29
//
//    let betaTurn29 (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.betaTurn29
//
//    let betaSheet (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.betaSheet
//
//    let coil (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.coil
//
//    let normFrequencyBetaSheet (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.normFrequencyBetaSheet
//
//    let totalBetaStrandConfPreference (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.totalBetaStrandConfPreference
//
//    let parallelBetaStrandConfPreference (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.parallelBetaStrandConfPreference
//
//    let aaCompUniSwissProt (aas:AminoAcidSeq<_>) =
//            aas
//            |> Seq.map AminoAcids.toLiteral
//            |> Seq.map AminoAcidLiteral.Propteries.aaCompUniSwissProt



    
    /// Implementation of commonly used converters char -> Bioitem
    module OptionConverter = 
        
        /// Converters char to AminoAcid option by ignoring bad character
        let charToOptionAminoAcid (aac:char) =
            let pac = AminoAcidLiteral.charToParsedAminoAcidChar aac
            match pac with
            | AminoAcidLiteral.ParsedAminoAcidChar.StandardCodes  (aa) -> Some (AminoAcids.AminoAcid.Literal aa)
            | AminoAcidLiteral.ParsedAminoAcidChar.AmbiguityCodes (aa) -> Some (AminoAcids.AminoAcid.Literal aa) 
            | AminoAcidLiteral.ParsedAminoAcidChar.BadChar (c)         -> None                

        
        /// Converters char to AminoAcid option by ignoring bad character and ambiguis code
        let charToOptionStandardAminoAcid (aac:char) =
            let pac = AminoAcidLiteral.charToParsedAminoAcidChar aac
            match pac with
            | AminoAcidLiteral.ParsedAminoAcidChar.StandardCodes  (aa) -> Some (AminoAcids.AminoAcid.Literal aa)
            | AminoAcidLiteral.ParsedAminoAcidChar.AmbiguityCodes (aa) -> None
            | AminoAcidLiteral.ParsedAminoAcidChar.BadChar (c)         -> None


        /// Converters char to AminoAcid option by ignoring bad character
        let charToOptionNucleotid (nuc:char) =
            let pnc = NucleotideLiteral.charToParsedNucleotideChar nuc
            match pnc with
            | NucleotideLiteral.ParsedNucleotideChar.StandardCodes    (n) -> Some (Nucleotides.Literal n)
            | NucleotideLiteral.ParsedNucleotideChar.Standard_DNAonly (n) -> Some (Nucleotides.Literal n)
            | NucleotideLiteral.ParsedNucleotideChar.Standard_RNAonly (n) -> Some (Nucleotides.Literal n)
            | NucleotideLiteral.ParsedNucleotideChar.AmbiguityCodes   (n) -> Some (Nucleotides.Literal n)
            | NucleotideLiteral.ParsedNucleotideChar.BadChar (c)         -> None              


        /// Converters char to AminoAcid option by ignoring bad character
        let charToOptionStandardNucleotid (nuc:char) =
            let pnc = NucleotideLiteral.charToParsedNucleotideChar nuc
            match pnc with
            | NucleotideLiteral.ParsedNucleotideChar.StandardCodes    (n) -> Some (Nucleotides.Literal n)
            | NucleotideLiteral.ParsedNucleotideChar.Standard_DNAonly (n) -> Some (Nucleotides.Literal n)
            | NucleotideLiteral.ParsedNucleotideChar.Standard_RNAonly (n) -> Some (Nucleotides.Literal n)
            | NucleotideLiteral.ParsedNucleotideChar.AmbiguityCodes   (n) -> None
            | NucleotideLiteral.ParsedNucleotideChar.BadChar (c)          -> None  
        

#if INTERACTIVE
    module InstallFsiAutoDisplay =
        // Single
        fsi.AddPrinter( fun (nuc:Nucleotides.Nucleotide) -> (Nucleotides.symbol nuc).ToString() )
        fsi.AddPrinter( fun (aa:AminoAcids.AminoAcid)    -> (AminoAcids.symbol aa).ToString() )
    
//        // Sequences
//        fsi.AddPrinter( fun (nucs:BioSequences. NUC.NucleotideSequence) -> new string [|for n in nucs  -> Nucleotides.symbol n|] )
//        fsi.AddPrinter( fun (aas:BioSequences.AAS.AminoAcidSequence)   -> new string [|for a in aas   -> AminoAcids.symbol a |] )
//        fsi.AddPrinter( fun (bs:BioSequences.BioSequence)          -> BioSequences.toString bs )

        // other
        fsi.AddPrinter( fun (forumla:Formula.Formula) -> Formula.toString forumla )
    

#endif
