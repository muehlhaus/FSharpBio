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







(**

Converting a peptide string to a bio sequence

**)

let peptide = 
    "REYAHMIGMEYDTVQK"
    |> BioSequences.ofAminoAcidString OptionConverter.charToOptionAminoAcid


let peptide_15N =
    peptide
    |> Seq.map (fun acc -> AminoAcids.setIsotopicLabel IsotopicLabels.Table.N15_Label acc)
    |> fun a -> BioSequences.BioSeq<AminoAcids.AminoAcid>(a)

BioSequences.toStringProteinPilot peptide_15N


Mz.Fragmentation.abcSeries (peptide_15N |> List.ofSeq)


//let lSeq = 
//        seq
//        |> BioSequences.ofAminoAcidString OptionConverter.charToOptionAminoAcid
//        |> Seq.map (fun acc -> AminoAcids.setIsotopicLabel IsotopicLabels.Table.N15_Label acc)
//        |> fun a -> BioSequences.BioSeq<AminoAcids.AminoAcid>(a)



let getFragmentMz frg_type frg_nr frg_z sequence =
    let l = sequence |> List.ofSeq
    match frg_type with
    | 'b' -> Mz.Fragmentation.abcSeries l
             |> Seq.nth (frg_nr - 1) 
    | 'y' -> Mz.Fragmentation.xyzSeries l
             |> Seq.nth (l.Length - frg_nr )
    | _   -> failwithf "Fragment type unknown. %c" frg_type
    
    |> fun (_,f,_) -> Mz.Mass.toMZ f (float frg_z)


getFragmentMz 'y' 6 1 peptide_15N
getFragmentMz 'y' 6 1 peptide


Mz.Fragmentation.xyzSeries (peptide |> List.ofSeq) 

//(hash AminoAcidLiteral.Arg) ^^^ (hash AminoAcidLiteral.Arg)
//
//
// 
//
//
//let hashAacLiteralSeq (aacs: seq<AminoAcidLiteral.AminoAcidLiteral>) =
//    let (hash1,hash2,_) =
//        aacs
//        |> Seq.fold (fun (hash1,hash2,h) elem 
//                        -> let h' = hash elem
//                           let hash1 = ((hash1 <<< 5) + hash1) ^^^ h'
//                           let hash2 = ((hash2 <<< 5) + hash2) ^^^ h
//                           (hash1,hash2,h') ) (5381,5381,0)
//    hash1 + (hash2 * 1566083941)




//hashAacLiteralSeq [AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;] 
let a = [AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;] 
let b = [AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;AminoAcidLiteral.Arg;] 
//a.GetHashCode()
//b.GetHashCode()



//let amino1 = 

let aas1 = "PEPTIDE" |> Seq.choose OptionConverter.charToOptionAminoAcid
let aas2 = "PEPTIDE" |> Seq.choose OptionConverter.charToOptionAminoAcid

aas1 = aas2

let peptide1 = BioSequences.ofAminoAcidString OptionConverter.charToOptionAminoAcid "PEPTIDE"
let peptide2 = BioSequences.ofAminoAcidString OptionConverter.charToOptionAminoAcid "PEPTIDE"
peptide1 = peptide2


//let protein = BioSequences.ofAminoAcidString OptionConverter.charToOptionAminoAcid     """PEPTIDEKR
//PEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKR
//PEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKRPEPTIDEKR
//"""


let protein = BioSequences.ofAminoAcidString OptionConverter.charToOptionAminoAcid     """MSSEKMPIETLQADEHRFAIILGAGAAGIIQGCTFIREKTLPLEEFQILERQSAFGGVWWKNTYPGAACDIPSHEYQISF
ALNPYWSRTFAPQPEIQKYFEDVALQYELHKSTTFNTEIVEAKWDDSRLLWLVETTDLTTGDTKLWSCHVLIGALGAFTV
PKKAPVKNVDAFKGEEWHSVDWPKNANLKGKTVAVIGTGPSACQFIPNIYPEVKSLIVYQRSPGHVLPRNDVVVGSLTKW
MFAHIPFLMRFNRWFWMKKDEILRPRLFTVGSWLQKIVISMTRNHLYKQIKDDTLRRKLESKDVFGCKRPLMLSDYYPIF
NNDNVELVTDSVTELTENGIKSRNTDTGEEMERETDVLIWGTGYNPVDFGLPVPTKGRSGQLLCDKYQPELFSLYGVAVD
DFPNYFNFLGPNSSSFETSVMELFELQAHHNSIATEYLFQKNVGTFRYAIMPKEERVRSWTLSLRPGQAKLPPANPNCKS
YYRSKIGHVYRYPYPYWQYKALIAKLDFKRDWVLLQQRIGQKEVKVLEF
"""

BioSequences.toString protein




// Digestion

//let proteinDigestion = Mz.Digestion.cleave Mz.Digestion.Table.Trypsin 3 5 100 protein
let proteinDigestion = Mz.Digestion.cleave Mz.Digestion.Table.Trypsin 0 0 1000 protein

proteinDigestion
|> Seq.map Mz.Mass.monoIsotopicPeptideMassFrom |> Seq.nth 12


proteinDigestion |> Seq.distinct |> Seq.length




//
