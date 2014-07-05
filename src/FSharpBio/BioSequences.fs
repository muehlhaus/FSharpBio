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
    type BioSeq<'a when 'a :> IBioItem> 
        ( sequence  : seq<'a>,
          id        : string,
          displayId : string,
          metadata  : obj
        ) = 
        member internal this.asSeq = sequence
        member this.Id             = id
        member this.DisplayId      = displayId
        
        member this.MetaData       = metadata
        
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
