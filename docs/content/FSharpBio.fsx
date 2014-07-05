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


let peptide = BioSequences.ofAminoAcidString OptionConverter.charToOptionAminoAcid "PEPTIDE"

BioSequences.toString peptide






 