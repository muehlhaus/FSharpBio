namespace FSharpBio

module Modification =
     
    open AminoAcids
    open BioSequences
    open ModificationInfo
    open Digestion
    //open Fsharp.FsGenerics
    open IsotopicLabels
    open System



     /// Defines a class which maps amino acid symbols to modification infos
    type ModTable() = class      

        let fixedResidualMods = new System.Collections.Generic.Dictionary<char, ModificationResidual>()
        let fixedTerminalMods = new System.Collections.Generic.Dictionary<char * ModTerminalLocation, ModificationTerminal>()

        let variableResidualMods = new System.Collections.Generic.Dictionary<char, ModificationResidual list>()
        let variableTerminalMods = new System.Collections.Generic.Dictionary<char * ModTerminalLocation, ModificationTerminal list>()

        static member TryGet<'K, 'MT>(key:'K, dict:System.Collections.Generic.Dictionary<'K, 'MT>) =
            match dict.ContainsKey(key) with
                | true -> Some(dict.Item key)
                | false -> None

        static member private DictListAdd<'K, 'MT>(key:'K, item:'MT, dict:System.Collections.Generic.Dictionary<'K, 'MT list>) =
            match dict.ContainsKey(key) with
                | true -> dict.Add(key, item::(dict.Item key))
                | false -> dict.Add(key, [item])                       

        member this.AddFixedMod(name:string, generalName:string, formula:string, aminoAcidSymbol:char) =
            this.AddFixedMod(createModificationResidual name generalName (Formula.parseFormulaString(formula)) aminoAcidSymbol)

        member this.AddFixedMod(mr:ModificationResidual) = fixedResidualMods.Add(mr.AminoAcidSymbol, mr)

        member this.AddFixedMod(name:string, generalName:string, formula:string, aminoAcidSymbol:char, location:ModTerminalLocation) =
            this.AddFixedMod(createModificationTerminal name generalName (Formula.parseFormulaString(formula)) aminoAcidSymbol location)

        member this.AddFixedMod(mt:ModificationTerminal) = fixedTerminalMods.Add((mt.AminoAcidSymbol,mt.Location), mt)

        member this.AddVariableMod(name:string, generalName:string, formula:string, aminoAcidSymbol:char) =
            this.AddVariableMod(createModificationResidual name generalName (Formula.parseFormulaString(formula)) aminoAcidSymbol)        

        member this.AddVariableMod(mr:ModificationResidual) = ModTable.DictListAdd(mr.AminoAcidSymbol, mr, variableResidualMods) 

        member this.AddVariableMod(name:string, generalName:string, formula:string, aminoAcidSymbol:char, location:ModTerminalLocation) =
            this.AddVariableMod(createModificationTerminal name generalName (Formula.parseFormulaString(formula)) aminoAcidSymbol location)            

        member this.AddVariableMod(mt:ModificationTerminal) = ModTable.DictListAdd((mt.AminoAcidSymbol,mt.Location), mt, variableTerminalMods) 

        member this.GetFixedMod(symbol:char) = ModTable.TryGet(symbol, fixedResidualMods) 
        member this.GetFixedMod(symbol:char, location:ModTerminalLocation) = ModTable.TryGet((symbol, location), fixedTerminalMods) 

        member this.GetVariableMods(symbol:char) = ModTable.TryGet(symbol, variableResidualMods)
        member this.GetVariableMods(symbol:char, location:ModTerminalLocation) = ModTable.TryGet((symbol, location), variableTerminalMods) 

    end    

//    let varmodIndex(maxIndices:int []) =
//        let mutable max = 0
//        let range = [0 .. maxIndices.Length - 1]
//        for i in range do
//            max <- max + maxIndices.[i] * int(Math.Pow(float(maxIndices.[i]+1), float(i)))
//        max
        

    type private IndexCounter(maxIndices:int []) = class 

        let border = maxIndices
        let counter = Array.create border.Length 0
        let lastIdx = border.Length - 1
        let mutable isInitial = true;

        member this.Next() =                     
            let rec loop idx = 
                match counter.[idx] < border.[idx], idx < lastIdx with
                    | true, true -> Array.set counter idx (counter.[idx] + 1) 
                    | false, true -> Array.set counter idx 0
                                     loop (idx + 1)
                    | _, false -> Array.set counter idx (counter.[idx] + 1)
            if isInitial then
                isInitial <- false
                true
            else
                loop 0                   
                (counter.[lastIdx] <= border.[lastIdx])   
            
        member this.Current(idx:int) = counter.[idx]
                                                                                                        
    end                         

    type ModGenerator(modTable:ModTable) = class
        
        member this.ModTable with get() = modTable        

        static member IsModified (a:AminoAcid) = 
            match a with
            | AminoAcid.Literal (_)  -> false
            | AminoAcid.Isotopic (_) -> false
            | _                      -> true

        static member SetMod(aa:AminoAcid, mi:Modification) = 
            match aa with
            | AminoAcid.Literal (a)    -> AminoAcid.Modified (a,mi)
            | AminoAcid.Isotopic (a,i) -> AminoAcid.Both (a,i,mi)
            | _ -> aa            

        static member SetMods(aseq: seq<AminoAcid>, modInfos:seq<(int * Modification)>) = 
            // create aa positiion map
            let modMap = new System.Collections.Generic.Dictionary<int, Modification>()
            let ctermIdx = (Seq.length aseq) - 1
            for mi in modInfos do match mi with
                                  | (pos, m) ->  modMap.Add(pos, m)

            // define modfunction
            let modFun = fun idx aa -> match ModTable.TryGet(idx, modMap) with
                                        | Some(mi) -> ModGenerator.SetMod(aa, mi)
                                        | _ -> aa
            // set mods at positions
            Seq.mapi modFun aseq

        member private this.ModFixed (aa:AminoAcid, location:ModTerminalLocation) =   
            if ModGenerator.IsModified(aa) then
                aa
            else
                let resMod = this.ModTable.GetFixedMod(symbol aa)   
                let termMod = this.ModTable.GetFixedMod(symbol aa, location)              

                match resMod, termMod  with               
                    | Some(rm),Some(tm) -> ModGenerator.SetMod(aa, ModificationInfo.Both(rm, tm))            
                    | Some(rm),None -> ModGenerator.SetMod(aa, ModificationInfo.Residual( rm))
                    | None,Some(tm) -> ModGenerator.SetMod(aa,ModificationInfo.Terminal(tm))                                          
                    | _,_ -> aa
                
        member private this.ModFixed (aa:AminoAcid) =  
            if ModGenerator.IsModified(aa) then
                aa
            else           
                match this.ModTable.GetFixedMod(symbol aa) with
                    | Some(rm) -> ModGenerator.SetMod(aa,ModificationInfo.Residual( rm))
                    | None -> aa           

        member this.ModFixed(aseq: AminoAcid [], ntermMode:ModTerminalLocation, ctermMode:ModTerminalLocation) =
            let ctermIdx = aseq.Length - 1
            Array.mapi (fun idx aa -> if idx = 0 then this.ModFixed( aa, ntermMode)
                                      else if idx = ctermIdx then this.ModFixed( aa, ctermMode)
                                      else this.ModFixed (aa)) aseq        

        member private this.ModVariable (aa:AminoAcid, location:ModTerminalLocation) =
            if ModGenerator.IsModified(aa) then
                [| aa |]  
            else
                let resMods = this.ModTable.GetVariableMods(symbol aa) 
                let termMods = this.ModTable.GetVariableMods(symbol aa, location)
                match resMods, termMods with
                | Some(r),Some(t) -> seq { yield aa
                                           for rm in r do 
                                                            yield ModGenerator.SetMod(aa, ModificationInfo.Residual(rm))
                                                            for tm in t do 
                                                                yield ModGenerator.SetMod(aa,ModificationInfo.Terminal(tm))
                                                                yield ModGenerator.SetMod(aa,ModificationInfo.Both(rm,tm)) } 
                                    |> Seq.toArray
                | Some(r),None -> seq { yield aa
                                        for rm in r do yield ModGenerator.SetMod(aa,ModificationInfo.Residual(rm))} 
                                  |> Seq.toArray
                | None,Some(t) -> seq { yield aa
                                        for tm in t do yield ModGenerator.SetMod(aa,ModificationInfo.Terminal(tm))} 
                                  |> Seq.toArray
                | None, None -> [| aa |]                                             

        member private this.ModVariable (aa:AminoAcid) =
            if ModGenerator.IsModified(aa) then
                [| aa |] 
            else
                let resMods = this.ModTable.GetVariableMods(symbol aa) 
                match resMods with
                                | Some(r) -> seq {  yield aa
                                                    for rm in r do                                                 
                                                                yield ModGenerator.SetMod(aa, ModificationInfo.Residual(rm)) }
                                             |> Seq.toArray
                                | None -> [| aa |]                                                          
                 
        member this.ModVariable(proteinSequence:AminoAcid [], cli:CleavageIndex) = 
            seq {
                // create modification matrix           
                let modAAMatrix = Array.create cli.SequenceLength Array.empty<AminoAcid>
                // handle n/cterm part of peptide
                Array.set modAAMatrix (cli.StartIndexMapped) (this.ModVariable(proteinSequence.[cli.StartIndex], ModTerminalLocation.Nterm))
                Array.set modAAMatrix (cli.EndIndexMapped) (this.ModVariable(proteinSequence.[cli.EndIndex], ModTerminalLocation.Cterm))
                // handle inner part of peptide
                for aaidx = cli.StartIndex + 1 to cli.EndIndex - 1 do
                    Array.set modAAMatrix (cli.MapIndex(aaidx)) (this.ModVariable (proteinSequence.[aaidx]))
            
                // create modified peptide sequence variants according to variable modifications
                let indexBorders = Array.map (fun (a:AminoAcid []) -> (a.Length - 1)) modAAMatrix
                let ic = new IndexCounter(indexBorders);
            
                let selectFun = fun (idx:int) (aas:AminoAcid []) -> aas.[ic.Current(idx)]
            
                while ic.Next() do yield Array.mapi selectFun modAAMatrix
            }
                                      

    end

