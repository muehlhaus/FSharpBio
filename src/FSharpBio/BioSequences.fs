namespace FSharpBio

module BioSequences = 
    
    open System.Collections
    open System.Collections.Generic

//    ///http://stackoverflow.com/questions/8438244/implementing-ienumerablet-when-base-class-implements-ienumerable
//    let mkEnum<'T>(ie : System.Collections.IEnumerable) 
//                           : System.Collections.Generic.IEnumerator<'T> =
//        (seq { for x in ie -> unbox x }).GetEnumerator()

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
         
    
    ///
    let ofAminoAcidString (converter:bioItemOptionConverter<AminoAcids.AminoAcid>) (s:string) =          
        let aas = s |> Seq.choose converter
        BioSeq aas
    
//    // todo: refactor charToOptPac out -> make it general
//    let ofAminoAcidString (s:string) =
//        let charToOptPac (aac:char) =
//            let pac = AminoAcidLiteral.charToParsedAminoAcidChar aac
//            match pac with
//            | AminoAcidLiteral.ParsedAminoAcidChar.StandardCodes  (aa) -> Some (AminoAcids.Literal aa)
//            | AminoAcidLiteral.ParsedAminoAcidChar.AmbiguityCodes (aa) -> Some (AminoAcids.Literal aa) 
//            | AminoAcidLiteral.ParsedAminoAcidChar.BadChar (c)         -> None                
//        let aas = s |> Seq.choose charToOptPac           
//        BioSeq aas


    // todo: refactor charToOptPac out -> make it general
    let ofNucleotideString (s:string) =
        let charToOptPnc (nuc:char) =
            let pnc = NucleotideLiteral.charToParsedNucleotideChar nuc
            match pnc with
            | NucleotideLiteral.ParsedNucleotideChar.StandardCodes    (n) -> Some (Nucleotides.Literal n)
            | NucleotideLiteral.ParsedNucleotideChar.Standard_DNAonly (n) -> Some (Nucleotides.Literal n)
            | NucleotideLiteral.ParsedNucleotideChar.Standard_RNAonly (n) -> Some (Nucleotides.Literal n)
            | NucleotideLiteral.ParsedNucleotideChar.AmbiguityCodes   (n) -> Some (Nucleotides.Literal n)
            | NucleotideLiteral.ParsedNucleotideChar.BadChar (c)         -> None                

        let nucs = s |> Seq.choose charToOptPnc           
        BioSeq nucs

