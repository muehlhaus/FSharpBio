namespace FSharpBio

/// Representation and basic functions on nucleotides.
/// Nucleotides are organic molecules that serve as the monomers, or subunits, of nucleic acids like DNA and RNA.
module Nucleotides =
    
    open NucleotideLiteral

    /// Marker interface for generic nucleotides
    type INucleotides =
        inherit IBioItem
        abstract member ToLiteral : NucleotideLiteral


    /// Basic nucleotide
    type Nucleotide =
        | Literal  of NucleotideLiteral            
        static member private getLiteral (nuc:Nucleotide) = 
            match nuc with
            | Nucleotide.Literal (nuc)    -> nuc

        interface INucleotides with            
            member this.ToLiteral = this |> Nucleotide.getLiteral
            member this.Name      = this |> Nucleotide.getLiteral  |> Properties.name
            member this.Symbol    = this |> Nucleotide.getLiteral  |> Properties.symbol
            member this.ByteCode  = this |> Nucleotide.getLiteral  |> Properties.symbol |> byte
            member this.Formula   = this |> Nucleotide.getLiteral  |> Properties.formula

            
    
    /// 
    let isEqual (nucA:Nucleotide) (nucB:Nucleotide) =            
        let symbolA = (nucA:>IBioItem).Symbol
        let symbolB = (nucB:>IBioItem).Symbol
        symbolA.Equals(symbolB)



    /// Returns nucleotide formula
    let formula (nuc:Nucleotide) = 
        (nuc:>IBioItem).Formula

    /// Returns the symbol of Nucleotide
    let symbol  (nuc:Nucleotide) = 
        (nuc:>IBioItem).Symbol

    
    /// Returns the symbol of Nucleotide
    let toChar (nuc:Nucleotide) = 
        symbol nuc
