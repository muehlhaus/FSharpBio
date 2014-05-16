namespace FSharpBio

module BioSequences = 
    
    open System
    open System.Collections
    open System.Collections.Generic
    open AminoAcids
    open Nucleotides

    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type Stack<'a> = 
        | StackNode of 'a list
        with
            member private t.StructuredFormatDisplay = 
                if t.length = 0 then "()"
                else
                    let str = t |> Seq.fold (fun st e -> st + e.ToString() + "; ") "("
                    str.Substring(0, str.Length - 2) + ")"
 
            member t.length =
                t |> Seq.length
 
            member internal t.asList = 
                match t with StackNode(x) -> x
 
            member t.isEmpty = t.length = 0
 
            interface IEnumerable<'a> with
                member x.GetEnumerator() = (x.asList |> List.toSeq).GetEnumerator()
 
            interface IEnumerable with
                member x.GetEnumerator() =  (x.asList |> List.toSeq).GetEnumerator() :> IEnumerator

    ///http://stackoverflow.com/questions/8438244/implementing-ienumerablet-when-base-class-implements-ienumerable
    let mkEnum<'T>(ie : System.Collections.IEnumerable) 
                           : System.Collections.Generic.IEnumerator<'T> =
        (seq { for x in ie -> unbox x }).GetEnumerator()


    //https://github.com/fsharp/fsharp/blob/master/src/fsharp/FSharp.Core/seq.fs

    type BioItem = 
        | AminoAcidLiteral of AminoAcidLiteral
        | NucleotideLiteral of NucleotideLiteral


    type BioSeq = 
        | Protein of AminoAcidLiteral seq
        | DNA     of NucleotideLiteral seq
        with
 
            member internal this.asSeq =                                 
                match this with 
                | Protein(x) -> seq [ for a in x do yield BioItem.AminoAcidLiteral a ] 
                | DNA    (x) -> seq [ for a in x do yield BioItem.NucleotideLiteral a ] 
 
 
            interface IEnumerable<BioItem> with
                member this.GetEnumerator() = this.asSeq.GetEnumerator()
 
            interface IEnumerable with
                member this.GetEnumerator() =  this.asSeq.GetEnumerator() :> IEnumerator






//    type BioSeq = 
//        | Protein of AminoAcidLiteral  seq
//        | DNA     of NucleotideLiteral seq


    let peptide1 = BioSeq.Protein [AminoAcidLiteral.Ala;AminoAcidLiteral.Ala]

    let rna1 = BioSeq.DNA [NucleotideLiteral.A;NucleotideLiteral.A]

    let tmp = seq  [ for i in peptide1 do yield i]

//    type TraceBuilder() =
//        member this.Bind(m, f) = 
//            match m with 
//            | Protein (x) -> 
//                printfn "Binding with None. Exiting."
//            | DNA a -> 
//                printfn "Binding with Some(%A). Continuing" a
//            Option.bind f m
//
//        member this.Return(x) = 
//            printfn "Returning a unwrapped %A as an option" x
//            Some x
//
//        member this.ReturnFrom(m) = 
//            printfn "Returning an option (%A) directly" m
//            m
//
//    // make an instance of the workflow 
//    let trace = new TraceBuilder()