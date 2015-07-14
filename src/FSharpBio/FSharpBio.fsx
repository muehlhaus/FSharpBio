#I "D:\\Development\\Source\\FSharpBio\\bin"

#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.Fsharp.dll"

#r "FSharp.CoreX.dll"
#r "FSharpBio.dll"
#r "FSharp.Charting.dll"


module InstallFsiAutoDisplay =
    
    open FSharpBio
    
    // Single
    fsi.AddPrinter( fun (nuc:Nucleotides.Nucleotide) -> (Nucleotides.symbol nuc).ToString() )
    fsi.AddPrinter( fun (aa:AminoAcids.AminoAcid)    -> (AminoAcids.symbol aa).ToString() )
    
//        // Sequences
//        fsi.AddPrinter( fun (nucs:BioSequences. NUC.NucleotideSequence) -> new string [|for n in nucs  -> Nucleotides.symbol n|] )
//        fsi.AddPrinter( fun (aas:BioSequences.AAS.AminoAcidSequence)   -> new string [|for a in aas   -> AminoAcids.symbol a |] )
//        fsi.AddPrinter( fun (bs:BioSequences.BioSequence)          -> BioSequences.toString bs )

    // other
    fsi.AddPrinter( fun (forumla:Formula.Formula) -> Formula.toString forumla )
