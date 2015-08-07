(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
FSharpBio: Documentation
========================

FSharpBio aims to be a user-friendly library for Bioinformatics written in F#. It contains the basic data 
structures for common biological objects like amino acids and nucleotides based on chemical formulas and chemical elements. 
It facilitates some basic machine learning task as well as statistical analysis of biological data set.

Example
-------

This example demonstrates using a function defined in FSharpBio library.

*)
#r "FSharpBio.dll"
open FSharpBio

(**

Converting a peptide string to a biosequence

*)

let peptide1 = 
    "REYAHMIGMEYDTVQK"
    |> BioSequences.ofAminoAcidString

let peptide2 = 
    "REYAHMIGMEYDTVQK"
    |> BioSequences.ofAminoAcidString



let fAlanin = Formula.parseFormulaString "C3H5ON" 

fAlanin |> Formula.monoisoMass

fAlanin |> Formula.averageMass

Formula.add fAlanin Formula.Table.H2O  |> Formula.monoisoMass



let carboxyAmidoMethylation = ModificationInfo.createModificationWithAdd "CarboxyAmidoMethylation" ModificationInfo.ModLocation.Residual  (Formula.parseFormulaString "CH3")
 
//Carboxyamidomethylated Cysteine

AminoAcids.Cys
|> AminoAcids.setModification carboxyAmidoMethylation
|> AminoAcids.setModification carboxyAmidoMethylation
|> AminoAcids.formula


