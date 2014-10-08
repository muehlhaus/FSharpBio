namespace FSharpBio.Mz

open FSharpBio
open System.Text.RegularExpressions

module Digestion = 


    /// Class represents a protease with name of enzyme, regex cutting expression, formula c-terminal mofication
    /// formula n-terminal mofication, cutting side before AS, cutting side after AS and enzyme number
    type Protease
        ( name                  : string,
          expression            : string,
          cTerm                 : string,
          nTerm                 : string,
          cutBefore             : bool,
          cutAfter              : bool,
          number                : int ) = class

        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with
                | :? Protease as y -> compare x.Name y.Name
                | _ -> invalidArg "yobj" "cannot compare values of different types"

        override x.Equals(yobj) =
            match yobj with
            | :? Protease as y -> (x.Name = y.Name)
            | _ -> false
 
        override x.GetHashCode() = hash x.Name
        
        member this.Name            = name
        member this.Expression      = new Regex(expression,RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
        member this.cTerm           = cTerm
        member this.nTerm           = nTerm
        member this.CutBefore       = cutBefore
        member this.CutAfter        = cutAfter
        member this.Number          = number


    end

    type CleavageIndex(startIdx:int, endIdx:int, missCleavages:int) = class

        member this.StartIndex with get() = startIdx
        member this.EndIndex with get() = endIdx
        member this.MissCleavages with get() = missCleavages

        member this.SequenceLength = (this.EndIndex - this.StartIndex) + 1

        member this.StartIndexMapped with get() = 0
        member this.EndIndexMapped with get() = this.MapIndex(this.EndIndex)
        member this.MapIndex(idx:int) = idx - this.StartIndex

        member this.Copy(sourceSeq:AminoAcids.AminoAcid [], targetSeq:AminoAcids.AminoAcid [], startInSource:int, countFromSource:int) =
            for aaidx = startInSource to (startInSource + countFromSource) - 1 do
                    Array.set targetSeq (this.MapIndex(aaidx)) (sourceSeq.[aaidx])
    end

    
    /// Returns cleavage index
    let matchIndices (protease : Protease) (maxCleavages:int) (minLength:int) (maxLength:int) (aminoAcidSequence: seq<AminoAcids.AminoAcid>) =    
        seq {
            let stringSequence = new string [|for c in aminoAcidSequence -> AminoAcids.symbol c|]
            let matches = protease.Expression.Matches(stringSequence)        
            let cuttingIndices = seq {
                                    for i = 0 to matches.Count - 1 do 
                                        yield (matches.[i].Index,matches.[i].Index + matches.[i].Length - 1)
                                    } |> Seq.toArray                              
        
        
            for cl = 0 to maxCleavages do
                for count = 0 to cuttingIndices.Length-1 do
                    if cl + count < cuttingIndices.Length then
                        let ci = new CleavageIndex(fst(cuttingIndices.[count]), snd(cuttingIndices.[cl + count]), cl)      
                        let len = ci.SequenceLength       
                        if (len >= minLength && len <= maxLength) then
                            yield ci
        }

    /// Reurns a sequence of sub sequences after cleavage
    // TODO: efficiency
    let cleave (protease : Protease) (maxCleavages:int) (minLength:int) (maxLength:int) (aminoAcidSequence: seq<AminoAcids.AminoAcid>) =    
        seq {
            let stringSequence = new string [|for c in aminoAcidSequence -> AminoAcids.symbol c|]
            let matches = protease.Expression.Matches(stringSequence)        
            let cuttingIndices = seq {
                                    for i = 0 to matches.Count - 1 do 
                                        yield (matches.[i].Index,matches.[i].Index + matches.[i].Length - 1)
                                    } |> Seq.toArray                              
        
        
            for cl = 0 to maxCleavages do
                for count = 0 to cuttingIndices.Length-1 do
                    if cl + count < cuttingIndices.Length then
                        let ci = new CleavageIndex(fst(cuttingIndices.[count]), snd(cuttingIndices.[cl + count]), cl)      
                        let len = ci.SequenceLength       
                        if (len >= minLength && len <= maxLength) then
                            yield BioSequences.ofAminoAcidString BioSequences.OptionConverter.charToOptionAminoAcid (stringSequence.Substring(ci.StartIndex,ci.SequenceLength))                            
                                    }


    // ##### ##### ##### ##### #####
    module Table = 

        // Definition of proteasis
        let Trypsin = new Protease("Trypsin", @"(.*?[KR](?!P)|.+)", "OH", "H", true, false, 0);
        let TrypsinP = new Protease("Trypsin/P", @"(.*?[KR]|.+)", "OH", "H", true, false, 1);
        let GluC = new Protease("Glu-C", @"(.*?[E](?!P)|.+)", "OH", "H", true, false, 2);

        /// Returns element object according to element symbol string
        let ProteaseAsObject (proteaseNameString:string) =
            match proteaseNameString with
            | "Trypsin"         -> Trypsin
            | "Trypsin/P"       -> TrypsinP
            | "Glu-C"           -> GluC

            | _ -> raise (System.ArgumentException("Protease unknown"))
