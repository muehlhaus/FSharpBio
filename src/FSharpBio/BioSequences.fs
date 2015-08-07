namespace FSharpBio

module BioSequences =

    module OptionConverter =  
        
        /// Type abbreviation for converting char to optional Nucleotide
        type NucleotideOptionConverter = char -> Nucleotides.Nucleotide option
        /// Type abbreviation for converting char to optional AminoAcid
        type AminoAcidOptionConverter = char -> AminoAcids.AminoAcid option

        /// Converters char to AminoAcid option by ignoring bad character
        let charToOptionAminoAcid (aac:char) =
            let pac = AminoAcids.charToParsedAminoAcidChar aac
            match pac with
            | AminoAcids.ParsedAminoAcidChar.StandardCodes  (aa) -> Some aa
            | AminoAcids.ParsedAminoAcidChar.AmbiguityCodes (aa) -> Some aa 
            | AminoAcids.ParsedAminoAcidChar.NoAAChar (_)        -> None                

        
        /// Converters char to AminoAcid option by ignoring bad character and ambiguis code
        let charToOptionStandardAminoAcid (aac:char) =
            let pac = AminoAcids.charToParsedAminoAcidChar aac
            match pac with
            | AminoAcids.ParsedAminoAcidChar.StandardCodes  (aa) -> Some aa
            | AminoAcids.ParsedAminoAcidChar.AmbiguityCodes (_)  -> None
            | AminoAcids.ParsedAminoAcidChar.NoAAChar (_)        -> None

        /// Converters char to AminoAcid option by ignoring bad character
        let charToOptionNucleotid (nuc:char) =
            let pnc = Nucleotides.charToParsedNucleotideChar nuc
            match pnc with
            | Nucleotides.ParsedNucleotideChar.StandardCodes    (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.Standard_DNAonly (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.Standard_RNAonly (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.AmbiguityCodes   (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.NoNucChar (_)        -> None              


        /// Converters char to AminoAcid option by ignoring bad character
        let charToOptionStandardNucleotid (nuc:char) =
            let pnc = Nucleotides.charToParsedNucleotideChar nuc
            match pnc with
            | Nucleotides.ParsedNucleotideChar.StandardCodes    (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.Standard_DNAonly (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.Standard_RNAonly (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.AmbiguityCodes   (_) -> None
            | Nucleotides.ParsedNucleotideChar.NoNucChar (_)        -> None  

    /// Generates amino acid sequence of one-letter-code string using given OptionConverter
    let ofAminoAcidStringWithOptionConverter (converter:OptionConverter.AminoAcidOptionConverter) (s:string) =          
        s |> Seq.choose converter

    /// Generates amino acid sequence of one-letter-code raw string
    let ofAminoAcidString (s:string) =          
        s |> Seq.choose OptionConverter.charToOptionAminoAcid


    /// Generates nucleotide sequence of one-letter-code string using given OptionConverter
    let ofNucleotideStringWithOptionConverter (converter:OptionConverter.NucleotideOptionConverter) (s:string) =             
        s |> Seq.choose converter           
        
    /// Generates nucleotide sequence of one-letter-code raw string
    let ofNucleotideString (s:string) =             
        s |> Seq.choose OptionConverter.charToOptionNucleotid           
        

    
    ///Active pattern which returns a base triplet
    let private (|Triplet|_|) (en:System.Collections.Generic.IEnumerator<_>) = 
        if en.MoveNext () then                
                    let n1 = en.Current
                    if en.MoveNext () then
                        let n2 = en.Current
                        if en.MoveNext () then
                            Some((n1,n2,en.Current))
                        else
                            None
                    else
                        None
                    
        else
            None

    //  Replace T by U
    /// Transcribe a given DNA coding strand (5'-----3')
    let transcribeCodeingStrand (nucs:seq<Nucleotides.Nucleotide>) = 
        nucs |> Seq.map (fun nuc -> Nucleotides.replaceTbyU nuc)
        


    //  
    /// Transcribe a given DNA template strand (3'-----5')
    let transcribeTemplateStrand (nucs:seq<Nucleotides.Nucleotide>) = 
        nucs |> Seq.map (fun nuc -> Nucleotides.replaceTbyU (Nucleotides.complement nuc))


    /// translates nucleotide sequence to aminoacid sequence    
    let translate (nucleotideOffset:int) (rnaSeq:seq<Nucleotides.Nucleotide>) =         
        let translateCodons (ns:seq<Nucleotides.Nucleotide>) = 
            let sourceIsEmpty = ref false    
            seq {   use en = ns.GetEnumerator()
                    while not(!sourceIsEmpty) do                
                    match en with
                    | Triplet t -> yield (Nucleotides.lookupBytes t)                                                              
                    | _         -> sourceIsEmpty := true                               
            }
        if (nucleotideOffset < 0) then
                raise (System.ArgumentException(sprintf "Input error: nucleotide offset of %i is invalid" nucleotideOffset))                
        rnaSeq
        |> Seq.skip nucleotideOffset
        |> translateCodons

    
    /// Compares the elemens of two biosequence
    let isEqual a b =
        let tmp = Seq.compareWith (fun elem1 elem2 ->
                            if elem1 = elem2 then 0    
                            else 1)  a b 
        tmp = 0

//
//
///// Returns string of one-letter-code
//    let toString (bs:BioSeq<_>) =
//        new string [|for c in bs  -> BioItem.symbol c|]         
//
//
//       
    /// Returns formula
    let toFormula (bs:seq<#IBioItem>) =
        bs |> Seq.fold (fun acc item -> Formula.add acc  (BioItem.formula item)) Formula.emptyFormula
//        
//
//
//    /// Returns string of one-letter-code with modifications and label
//    /// representation equivalent to ABSciex protein pilot software
//    let toStringProteinPilot (bs:BioSeq<_>) =
//        let getString (b:#IBioItem) =            
//            match box b with
//            | :? AminoAcids.AminoAcid     as a -> let massDiffInt = AminoAcids.monoisoMassDiff a |> round |> int                                                   
//                                                  sprintf "%c[%0+3i]" (AminoAcids.toChar a) massDiffInt
//            | :? Nucleotides.INucleotides as n -> sprintf "%c" (BioItem.symbol n)
//            | _                                -> failwithf "Type is unknown: %A" b
//
//        new string [|for c in bs  do yield! (getString c)|]  

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
