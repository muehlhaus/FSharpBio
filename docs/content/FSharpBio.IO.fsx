(*** hide ***)
#I "../../bin"

(**
FSharpBio: Documentation
========================

FSharpBio aims to be a user-friendly library for Bioinformatics written in F#. It contains the basic data 
structures for common biological objects like amino acids and nucleotides based on chemical formulas and chemical elements. 
It facilitates some basic machine learning task as well as statistical analysis of biological data set.


This example demonstrates using a function defined in FSharpBio library.

*)
#r "FSharpBio.dll"
#r "FSharpBio.IO.dll"

open FSharpBio
open FSharpBio.IO

(**


Read a fastA file
----------------

Converter reads character and returns it as either amino acid or nucleotide
depending on the `OptionConverter` respectivly.
For a protein fastA use: `OptionConverter.charToOptionStandardAminoAcid` and 
for gene fastA use: `OptionConverter.charToOptionStandardNucleotid`.

*)


let converter = BioSequences.OptionConverter.charToOptionStandardAminoAcid

// Path to .fastA file
let fastaPath = __SOURCE_DIRECTORY__ + "/data/chlamy3proteins.fasta"
let chlamy3proteins =
    // Read .fastA
    FastA.fromFileWithOptional converter fastaPath
    |> Seq.toArray


(**
Digest proteins
---------------
Digests the proteins from .fastA file to peptides. 
Trypsin is used as the protease 
*)


let trypsinPeptides =
    chlamy3proteins
    |> Seq.map (fun fastaItem -> fastaItem.Sequence)    
    |> Seq.collect (fun aas -> Digestion.digest Digestion.trypsin aas)


(**
Mass calculation
----------------
Calculates peptide masses (monoisotopic) of previous digestion.

*)

let peptideMasses =
    trypsinPeptides
    |> Seq.map (fun peptide -> let fPEptide = BioSequences.toFormula peptide
                               Formula.add fPEptide Formula.Table.H2O
                               |> Formula.monoisoMass
                        )

(**
Mass histogram
----------------
Shows distribution of monoisotopic peptide masses

*)
(*** define-output:histogram ***)
#r "FSharp.Charting.dll"
#r "FSharpStats.dll"

open FSharpStats
open FSharp.Charting

let bw = 0.7
let histo = Descriptive.Histogram.create bw peptideMasses

Chart.Column (histo |> Descriptive.Histogram.getZip)
|> Chart.WithXAxis(Max=8000.,Min=300.)
(*** include-it:histogram ***)


//// Write
//let _ = chlamy3proteins |> FastA.write AminoAcids.symbol fastaPath






