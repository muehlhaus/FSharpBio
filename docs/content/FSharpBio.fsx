(*** hide ***)
#I "../../bin"





(**
Samples for FsharpBio Project
-----------------------------

Solum ipsum 

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
*)


#r "FSharp.CoreX.dll"
#r "FSharpBio.dll"

open FSharpBio
open FSharpBio.BioSequences





#if INTERACTIVE
    module InstallFsiAutoDisplay =
        // Single
        fsi.AddPrinter( fun (nuc:Nucleotides.Nucleotide) -> (Nucleotides.symbol nuc).ToString() )
        fsi.AddPrinter( fun (aa:AminoAcids.AminoAcid)    -> (AminoAcids.symbol aa).ToString() )
    
        // Sequences
        fsi.AddPrinter( fun (bs:BioSequences.BioSeq<_>)                -> BioSequences.toString bs )

        // other
        fsi.AddPrinter( fun (forumla:Formula.Formula) -> Formula.toString forumla )
    

#endif





(**

Converting a peptide string to a bio sequence

**)


//AminoAcids.isEqual
let a = "A"
let a1 = "A"


(hash AminoAcidLiteral.Arg) ^^^ (hash AminoAcidLiteral.Arg)


 


let hashAacLiteralSeq (aacs: seq<AminoAcidLiteral.AminoAcidLiteral>) =
    let (hash1,hash2,_) =
        aacs
        |> Seq.fold (fun (hash1,hash2,h) elem 
                        -> let h' = hash elem
                           let hash1 = ((hash1 <<< 5) + hash1) ^^^ h'
                           let hash2 = ((hash2 <<< 5) + hash2) ^^^ h
                           (hash1,hash2,h') ) (5381,5381,0)
    hash1 + (hash2 * 1566083941)




hashAacLiteralSeq [AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;] 
let a = [AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;] 
let b = [AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;] 
a.GetHashCode()
b.GetHashCode()


//let amino1 = 

let aas1 = "PEPTIDE" |> Seq.choose OptionConverter.charToOptionAminoAcid
let aas2 = "PEPTIDE" |> Seq.choose OptionConverter.charToOptionAminoAcid

aas1 = aas2

let peptide1 = BioSequences.ofAminoAcidString OptionConverter.charToOptionAminoAcid "PEPTIDE"
let peptide2 = BioSequences.ofAminoAcidString OptionConverter.charToOptionAminoAcid "PEPTIDE"
peptide1 = peptide2


let protein = BioSequences.ofAminoAcidString OptionConverter.charToOptionAminoAcid     """PEPTIDEKR
PEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKR
PEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKR
"""
BioSequences.toString protein




// Digestion

let proteinDigestion = Mz.Digestion.cleave Mz.Digestion.Table.Trypsin 3 5 100 protein

proteinDigestion
|> Seq.map Mz.Mass.monoIsotopicPeptideMassFrom


proteinDigestion |> Seq.distinct |> Seq.length
