namespace FSharpBio


/// Basic functions on IBioItems interface
module BioItem =

    
    /// Returns then name of the bio item
    let name (bItem:#IBioItem) =
        bItem.Name


    /// Returns then symbol of the bio item
    let symbol (bItem:#IBioItem) =
        bItem.Symbol


    /// Returns then byteCode of the bio item
    let byteCode (bItem:#IBioItem) =
        bItem.ByteCode


    /// Returns then byteCode of the bio item
    let formula  (bItem:#IBioItem) =
        bItem.Formula

    
    /// Basic functions on IAminoAcid interface
    module AminoAcid =
        
        /// Returns AminoAcidLiteral if input implements IAminoAcid interface
        let toLiteral (bItem:#AminoAcids.IAminoAcid) = 
            bItem.ToLiteral


    /// Basic functions on INucleotides interface
    module Nucleotides =
        
        /// Returns NucleotideLiteral if input implements INucleotides interface
        let toLiteral (bItem:#Nucleotides.INucleotides) =
            bItem.ToLiteral

