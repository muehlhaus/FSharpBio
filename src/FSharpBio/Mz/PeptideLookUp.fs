namespace FSharpBio.Mz

module PeptideLookUp =

    open FSharpBio
    open AminoAcids        
    open Modification
    open ModificationInfo
    open Digestion
    open System.IO    
    //open Fsharp.FsIO 
    open PMI
    open System.Collections.Generic         
    open System.Xml
    open System.Globalization

    type PeptideInfo = { CleavageIndex:CleavageIndex; ProteinID:string; Sequence:AminoAcid []} 

    type PeptideInfoGroup = 
        { ProteinCleavageInfo : list<string*Digestion.CleavageIndex>;      
          Sequence      : AminoAcids.AminoAcid [];
          Mass          : float }
    
    let createPeptideInfoGroup info sequence mass =
        { ProteinCleavageInfo = info; Sequence = sequence; Mass = mass; }

    type PeptideWriter(pepFilePath:string) = class
                  
        let bufSize = 16777216                 
        let fileStream = new BufferedStream( new FileStream(pepFilePath, FileMode.Create, FileAccess.Write, FileShare.None), bufSize)
        let buffer = Array.create bufSize (byte 0)
                   
        member this.WritePeptide(peptide:PeptideInfo) =   
        
            let ms = new MemoryStream(buffer)
            let writer = new BinaryWriter(ms)   

            let modInfoCollector = fun (idx:int) (aa:AminoAcid) -> match aa with                                                   
                                                                    | AminoAcid.Modified(l,m) -> Some((idx,m))
                                                                    | AminoAcid.Both(l,i,m) -> Some((idx,m))
                                                                    | _ -> None
                                             
            writer.Write(peptide.ProteinID)

            writer.Write(peptide.CleavageIndex.StartIndex)
            writer.Write(peptide.CleavageIndex.EndIndex)
            writer.Write(peptide.CleavageIndex.MissCleavages)
                        
            let modInfos = Seq.mapi modInfoCollector peptide.Sequence |> Seq.choose (fun x -> x)            
            writer.Write(Seq.length modInfos)
            for (idx, mi) in modInfos do
                writer.Write(idx)                       
                match (mi) with
                | ModificationInfo.Residual (mr) -> writer.Write('r') 
                                                    writer.Write(mr.Name)
                | ModificationInfo.Terminal (mt) -> writer.Write('t')
                                                    writer.Write(mt.Name)
                | ModificationInfo.Both (mr,mt)  -> writer.Write('b')
                                                    writer.Write(mr.Name)
                                                    writer.Write(mt.Name)
           
            writer.Flush()            

            let pos = fileStream.Position
            fileStream.Write(buffer, 0, int(ms.Position))            
            pos

        member this.Close() =
            fileStream.Flush()
            fileStream.Close()                          

    end

    type PeptideReader(pepFilePath:string, proteins:Map<string, AminoAcid []>, termMods:Map<string, ModificationTerminal>, resMods:Map<string, ModificationResidual>) = class
        
        let reader = new BinaryReader( new FileStream(pepFilePath, FileMode.Open, FileAccess.Read, FileShare.Read));
        let proteinMap = proteins
        let terminalMods = termMods
        let residualMods = resMods

        member this.FilePosition with get() = reader.BaseStream.Position
        member this.FileLength with get() = reader.BaseStream.Length
        member this.HasNext with get() = this.FilePosition < this.FileLength

        member this.ReadPeptide(seek:int64) =
            reader.BaseStream.Seek(seek, SeekOrigin.Begin) |> ignore
            this.ReadNext()
            
        member this.ReadAll() =
            this.Reset()
            seq {
                while this.HasNext do                    
                    yield this.ReadNext()
            }        

        member this.ReadNext() =            
            let protID = reader.ReadString()            
            let seqStart = reader.ReadInt32()
            let seqEnd = reader.ReadInt32()
            let missCl = reader.ReadInt32()
            let mods = this.ReadModInfos()
            
            let protSeq = proteinMap.[protID]
            let pepSeq = protSeq.[seqStart .. seqEnd]
            for mi in mods do
                Array.set pepSeq (fst mi) (ModGenerator.SetMod(pepSeq.[fst mi], snd mi))

            { CleavageIndex=new CleavageIndex(seqStart, seqEnd, missCl); ProteinID=protID; Sequence=pepSeq }        

        member private this.ReadModInfos() =
            let cnt = reader.ReadInt32()
            let readFun = fun (i:int) ->    let aa_idx = reader.ReadInt32()
                                            let typeID = reader.ReadChar()
                                            let name = reader.ReadString()
                                            if(typeID = 'r') then
                                                (aa_idx, ModificationInfo.Residual(residualMods.[name]))
                                            else if(typeID = 't') then
                                                (aa_idx, ModificationInfo.Terminal(terminalMods.[name]))
                                            else
                                                let name2  = reader.ReadString()
                                                (aa_idx, ModificationInfo.Both(residualMods.[name], terminalMods.[name]))
            if(cnt = 0) then
                Array.empty<(int * Modification)>
            else
                Array.init cnt readFun
        
        member this.Reset() = 
            reader.BaseStream.Seek(0L, SeekOrigin.Begin) |> ignore

        member this.Close() =            
            reader.Close()

    end             

    type MassMode = Avg = 0 | Mono = 1 

    type CachingMassFunc() = class 
        
        let literalMasses = new System.Collections.Generic.Dictionary<AminoAcidLiteral.AminoAcidLiteral, float>()         
        let avgmassFun = fun (aa:AminoAcid) -> Formula.averageMass (AminoAcids.formula aa)
        let monomassFun = fun (aa:AminoAcid) -> Formula.monoisoMass (AminoAcids.formula aa)
        let avgH2O =  Formula.averageMass Formula.Table.H2O 
        let monoH2O = Formula.monoisoMass Formula.Table.H2O

        member private this.TryGetOrAdd(aa : AminoAcidLiteral.AminoAcidLiteral, massFun:AminoAcid -> float) =
            let sym = aa
            if literalMasses.ContainsKey(sym) then
                 literalMasses.Item sym
            else
                let mass = massFun (AminoAcid.Literal(aa))
                literalMasses.Add(sym, mass)
                mass

        member private this.GetMass(aa:AminoAcid, massFun:AminoAcid -> float) =
            match aa with
            | Literal(l) -> this.TryGetOrAdd(l, massFun)
            | _ -> massFun aa

        /// Returns average mass of AminoAcidSequence including H20
        member this.AverageMass(s:seq<AminoAcid>) =
            s |> Seq.fold (fun acc aa -> acc + this.GetMass(aa, avgmassFun)) avgH2O

        /// Returns monoisotopic mass of AminoAcidSequence including H20
        member this.MonoisoMass(s:seq<AminoAcid>) =
            s |> Seq.fold (fun acc aa -> acc + this.GetMass(aa, monomassFun)) monoH2O

        member this.Mass(s:seq<AminoAcid>, mm:MassMode) =
            match mm with
            | MassMode.Mono -> this.MonoisoMass s
            | MassMode.Avg -> this.AverageMass s
            | _ -> raise (System.Exception("Unsupported mass mode: " + mm.ToString()))
    end    



    type PeptideDBParams(name:string, // name of database i.e. Creinhardtii_236_protein_full_labeled
                                dbPath:string,  // folder path where db files are stored
                                fastaPath:string,  // path to fasta file to digest i.e. C:\primaqdev\v5.3.fa
                                proteaseName:string,  // valid name of protease in protease table i.e. Trypsin/P
                                missCleavages:int, 
                                minPepLength:int, 
                                maxPepMass:float, 
                                isoLabelName:string, // valid symbol name of isotopic label in label table i.e. #N15
                                massMode:MassMode) = class 
               
        let mutable fixedResidualMods:list<ModificationResidual> = []
        let mutable fixedTerminalMods:list<ModificationTerminal> = []

        let mutable variableResidualMods:list<ModificationResidual> = []
        let mutable variableTerminalMods:list<ModificationTerminal> = []
        
        member this.FixedResidualMods with get() = fixedResidualMods
        member this.FixedTerminalMods with get() = fixedTerminalMods

        member this.VariableResidualMods with get() = variableResidualMods
        member this.VariableTerminalMods with get() = variableTerminalMods

        new(name:string, dbPath:string, fastaPath:string, proteaseName:string, missCleavages:int, minPepLength:int, maxPepMass:float, massMode:MassMode) = 
            new PeptideDBParams(name, dbPath, fastaPath, proteaseName, missCleavages, minPepLength, maxPepMass, "", massMode)

        member this.Name with get() = name
        member this.DBPath with get() = Path.GetFullPath(dbPath)
        member this.FastaPath with get() = Path.GetFullPath(fastaPath)
        member this.ProteaseName with get() = proteaseName        
        member this.MaximumMissedCleavages with get() = missCleavages
        member this.MinimumPepLength with get() = minPepLength
        member this.MaximumPepMass with get() = maxPepMass
        member this.IsotopicLabelName with get() = isoLabelName
        member this.MassMode with get() = massMode

        member this.PathToPeptideFile with get() = Path.Combine(dbPath, name + ".peptide")
        member this.PathToIndexFile with get() = Path.Combine(dbPath, name + ".idx")
        member this.PathToLabeledIndexFile with get() = Path.Combine(dbPath, name + ".idx.labeled")

        member this.IsLabeledMethod with get() = not (System.String.IsNullOrWhiteSpace( this.IsotopicLabelName))

        member this.AddFixedMod(name:string, generalName:string, formula:string, aminoAcidSymbol:char) =
            let mr = createModificationResidual name generalName (Formula.parseFormulaString(formula)) aminoAcidSymbol
            fixedResidualMods <- List.append fixedResidualMods [mr]

        member this.AddFixedMod(name:string, generalName:string, formula:string, aminoAcidSymbol:char, location:ModTerminalLocation) =
            let mt = createModificationTerminal name generalName (Formula.parseFormulaString(formula)) aminoAcidSymbol location
            fixedTerminalMods <- List.append fixedTerminalMods [mt]

        member this.AddVariableMod(name:string, generalName:string, formula:string, aminoAcidSymbol:char) =
            let mr = createModificationResidual name generalName (Formula.parseFormulaString(formula)) aminoAcidSymbol
            variableResidualMods <- List.append variableResidualMods [mr]

        member this.AddVariableMod(name:string, generalName:string, formula:string, aminoAcidSymbol:char, location:ModTerminalLocation) =
            let mt = createModificationTerminal name generalName (Formula.parseFormulaString(formula)) aminoAcidSymbol location
            variableTerminalMods <- List.append variableTerminalMods [mt]               
    end

    module PeptideDBParamsIO = 

        let private getRequiredAttribute(reader:XmlReader, name:string) =
            let value = reader.GetAttribute(name)
            if value = null then
                raise (System.Exception("Missing attribute: " + name))
            else
                value

        let private writeTermMod(writer:XmlWriter, m:ModificationTerminal) =
            writer.WriteStartElement("modTerminal")
            writer.WriteAttributeString("name", m.Name)
            writer.WriteAttributeString("generalName", m.GeneralName)
            writer.WriteAttributeString("formula", Formula.toString m.Formula)
            writer.WriteAttributeString("aminoAcid", m.AminoAcidSymbol.ToString())
            writer.WriteAttributeString("location", m.Location.ToString())
            writer.WriteEndElement()

        let private writeResMod(writer:XmlWriter, m:ModificationResidual) =
            writer.WriteStartElement("modResidual")
            writer.WriteAttributeString("name", m.Name)
            writer.WriteAttributeString("generalName", m.GeneralName)
            writer.WriteAttributeString("formula", Formula.toString m.Formula)
            writer.WriteAttributeString("aminoAcid", m.AminoAcidSymbol.ToString())            
            writer.WriteEndElement()

        let private readVarMods(pars:PeptideDBParams, reader:XmlReader) = 
            while reader.Read() do
                if reader.IsStartElement("modTerminal") then
                    let pos = ModTerminalLocation.Cterm
                    pars.AddVariableMod(getRequiredAttribute(reader, "name"), 
                                        getRequiredAttribute(reader, "generalName"), 
                                        getRequiredAttribute(reader, "formula"), 
                                        getRequiredAttribute(reader, "aminoAcid").[0], 
                                        pos)
                if reader.IsStartElement("modResidual") then
                    pars.AddVariableMod(getRequiredAttribute(reader, "name"), 
                                        getRequiredAttribute(reader, "generalName"), 
                                        getRequiredAttribute(reader, "formula"), 
                                        getRequiredAttribute(reader, "aminoAcid").[0])
            reader.Close()

        let private readFixedMods(pars:PeptideDBParams, reader:XmlReader) = 
            while reader.Read() do
                if reader.IsStartElement("modTerminal") then
                    let pos = ModTerminalLocation.Cterm
                    pars.AddFixedMod(getRequiredAttribute(reader, "name"), 
                                        getRequiredAttribute(reader, "generalName"), 
                                        getRequiredAttribute(reader, "formula"), 
                                        getRequiredAttribute(reader, "aminoAcid").[0], 
                                        pos)
                if reader.IsStartElement("modResidual") then
                    pars.AddFixedMod(getRequiredAttribute(reader, "name"), 
                                        getRequiredAttribute(reader, "generalName"), 
                                        getRequiredAttribute(reader, "formula"), 
                                        getRequiredAttribute(reader, "aminoAcid").[0])
            reader.Close()

        let ReadFromFile(path:string) = 
            let ci = new CultureInfo("en-US")
            let p = if path.EndsWith(".par") then path else path + ".par"
            let reader = XmlReader.Create(p)
            if reader.ReadToFollowing("peptide_db") then
                let label = if reader.GetAttribute("label") = null then "" else reader.GetAttribute("label")
                let massMode = if getRequiredAttribute(reader, "massMode") = MassMode.Mono.ToString() then MassMode.Mono else MassMode.Avg
                let par = new PeptideDBParams(getRequiredAttribute(reader, "name"), 
                                              getRequiredAttribute(reader, "dbPath"), 
                                              getRequiredAttribute(reader, "fastaPath"), 
                                              getRequiredAttribute(reader, "protease"), 
                                              System.Convert.ToInt32(getRequiredAttribute(reader, "missCleavages"), ci),
                                              System.Convert.ToInt32(getRequiredAttribute(reader, "minPepLength"), ci),
                                              System.Convert.ToDouble(getRequiredAttribute(reader, "maxPepMass"), ci),
                                              label, 
                                              massMode)
                
                if reader.ReadToFollowing("modifications") then
                    while reader.Read() do
                        if reader.IsStartElement("fixed") then
                            readFixedMods(par, reader.ReadSubtree())                            
                        if reader.IsStartElement("variable") then
                            readVarMods(par, reader.ReadSubtree())                                                  
                
                reader.Close()
                
                par
            else
                raise (System.Exception("Node <peptide_db> not found in parameter file."))                 

        let WriteToFile(pars:PeptideDBParams, path:string) = 
            let ci = new CultureInfo("en-US")
            
            let p = if path.EndsWith(".par") then path else path + ".par"    
            let writer = XmlWriter.Create(p)

            writer.WriteStartDocument()

            writer.WriteStartElement("peptide_db")
            writer.WriteAttributeString("name", pars.Name)
            writer.WriteAttributeString("dbPath", pars.DBPath)
            writer.WriteAttributeString("fastaPath", pars.FastaPath)
            writer.WriteAttributeString("protease", pars.ProteaseName)
            writer.WriteAttributeString("missCleavages", pars.MaximumMissedCleavages.ToString(ci))
            writer.WriteAttributeString("minPepLength", pars.MinimumPepLength.ToString(ci))
            writer.WriteAttributeString("maxPepMass", pars.MaximumPepMass.ToString(ci))
            writer.WriteAttributeString("label", pars.IsotopicLabelName)
            writer.WriteAttributeString("massMode", pars.MassMode.ToString())

            writer.WriteStartElement("modifications")

            writer.WriteStartElement("variable")
            for m in pars.VariableResidualMods do writeResMod(writer, m)
            for m in pars.VariableTerminalMods do writeTermMod(writer, m)
            writer.WriteEndElement() // variable

            writer.WriteStartElement("fixed")   
            for m in pars.FixedResidualMods do writeResMod(writer, m)
            for m in pars.FixedTerminalMods do writeTermMod(writer, m)         
            writer.WriteEndElement() // fixed
            
            writer.WriteEndElement() // modifications

            writer.WriteEndElement() // peptide_db
            
            writer.WriteEndDocument()

            writer.Flush()
            writer.Close()    

    module PeptideDBBuilder =

        let labelFun (sequence:AminoAcid [], label:IsotopicLabels.IsotopicLabel) =            
            let fn = fun (aa:AminoAcid) -> match aa with
                                                 | AminoAcid.Literal(lit) -> AminoAcid.Isotopic(lit, label)
                                                 | AminoAcid.Isotopic(lit, lbl) -> AminoAcid.Isotopic(lit, label)
                                                 | AminoAcid.Modified(lit, mi) -> AminoAcid.Both(lit, label, mi)
                                                 | AminoAcid.Both(lit, lbl, mi) -> AminoAcid.Both(lit, label, mi)
            Array.map fn sequence
        
        let private readProteinMapFromFasta (mth:PeptideDBParams) =
            
            let aaConverter = BioSequences.OptionConverter.charToOptionAminoAcid
            let proteins =  IO.FastA.fromFileWithOptional aaConverter mth.FastaPath |> Seq.map (fun (fi) -> (fi.Header, Seq.toArray fi.Sequence))
            
            new Map<string, AminoAcid []>(proteins)
        
        let private createModTable(mth:PeptideDBParams) =
            let mt = new ModTable()
            List.iter (fun (m:ModificationResidual) -> mt.AddFixedMod(m)) mth.FixedResidualMods
            List.iter (fun (m:ModificationTerminal) -> mt.AddFixedMod(m)) mth.FixedTerminalMods
            List.iter (fun (m:ModificationResidual) -> mt.AddVariableMod(m)) mth.VariableResidualMods
            List.iter (fun (m:ModificationTerminal) -> mt.AddVariableMod(m)) mth.VariableTerminalMods
            mt        
       
        /// estimate peptide length by average amino acid weight
        let private estimateMaxPeptideLength(maxPepMass:float) = int((maxPepMass / 128.0) * 1.3 )

        let private digestPeptides (proteinSequence:seq<AminoAcid>, proteinID:string, protease:Protease, missCleavages:int, minPepLength:int, maxPepLength:int, modTable:ModTable) = 
            seq {  
                let pgen = new ModGenerator(modTable)
                let modFixedSeq = pgen.ModFixed(Seq.toArray proteinSequence, ModTerminalLocation.ProteinNterm, ModTerminalLocation.ProteinCterm)            
                let clIndices = Digestion.matchIndices protease missCleavages minPepLength maxPepLength modFixedSeq
            
                for cli in clIndices do                                         
                    for modSeq in pgen.ModVariable(modFixedSeq, cli) do
                        yield { CleavageIndex=cli; ProteinID=proteinID; Sequence=modSeq }
            }                                             
        
        // check database exists
        // assume database exists if all required files exists
        let private dBExists(par:PeptideDBParams) = 
            if par.IsLabeledMethod then
                File.Exists(par.PathToPeptideFile) && File.Exists(par.PathToIndexFile) && File.Exists(par.PathToLabeledIndexFile)
            else                
                File.Exists(par.PathToPeptideFile) && File.Exists(par.PathToIndexFile)               

        let BuildPeptideDB (par:PeptideDBParams, forceRebuild:bool) = 
            
            if(dBExists(par) = false || forceRebuild) then

                if File.Exists(par.PathToPeptideFile) then File.Delete(par.PathToPeptideFile)
                if File.Exists(par.PathToIndexFile) then File.Delete(par.PathToIndexFile)
                if File.Exists(par.PathToLabeledIndexFile) then File.Delete(par.PathToLabeledIndexFile)

                if not (Directory.Exists(par.DBPath)) then Directory.CreateDirectory(par.DBPath) |> ignore
                
                let mutable pepMI:PeptideMassIndex = null
                let mutable pepMILabeled:PeptideMassIndex = null

                try 
                    let writer = PeptideWriter(par.PathToPeptideFile)
                    let proteins = readProteinMapFromFasta(par)                                                                 
                    pepMI <- PeptideMassIndex.Create(par.PathToIndexFile, 16) 
                    pepMILabeled <- if par.IsLabeledMethod then PeptideMassIndex.Create(par.PathToLabeledIndexFile, 16) else null
                    let maxPepLength = estimateMaxPeptideLength(par.MaximumPepMass)
                    let massFun = new CachingMassFunc()
                    let protease = Digestion.Table.ProteaseAsObject(par.ProteaseName)
                    let modTable = createModTable(par)
                    let label = if par.IsLabeledMethod then Some(IsotopicLabels.Table.SymbolAsObject par.IsotopicLabelName) else None

                    for prot in proteins do        
                        let peptides = digestPeptides( prot.Value, prot.Key, protease, par.MaximumMissedCleavages, par.MinimumPepLength, maxPepLength, modTable)
                        for pep in peptides do   
                            let mass = massFun.Mass(pep.Sequence, par.MassMode)
                            if mass <= par.MaximumPepMass then             
                                let seek = writer.WritePeptide(pep)                   
                                pepMI.Insert(mass, seek)
                                if par.IsLabeledMethod then
                                    let massLabeled = massFun.Mass (labelFun(pep.Sequence, label.Value), par.MassMode)
                                    pepMILabeled.Insert(massLabeled, seek)
                    writer.Close()
                finally                      
                    if pepMI <> null then pepMI.Shutdown()   
                    if pepMILabeled <> null then pepMILabeled.Shutdown()                                     

    open System.Linq    

    type PeptideQuery(cacheSize:int, pars:PeptideDBParams) = class
                  
        let cache = new System.Collections.Generic.Dictionary<int64, PeptideInfo>(cacheSize)
        let mutable massIndex:PeptideMassIndex = null
        let mutable labeledMassIndex:PeptideMassIndex = null

        let readProteinMapFromFasta (mth:PeptideDBParams) =
            let aaConverter = BioSequences.OptionConverter.charToOptionAminoAcid
            let proteins =  IO.FastA.fromFileWithOptional aaConverter mth.FastaPath |> Seq.map (fun (fi) -> (fi.Header, Seq.toArray fi.Sequence))
            new Map<string, AminoAcid []>(proteins)

        let createPeptideReader (mth:PeptideDBParams) =   
            let termMods = seq { yield! mth.FixedTerminalMods 
                                 yield! mth.VariableTerminalMods } |> Seq.map (fun (m) -> (m.Name, m))    
            let resMods = seq { yield! mth.FixedResidualMods 
                                yield! mth.VariableResidualMods } |> Seq.map (fun (m) -> (m.Name, m))    
            new PeptideReader(mth.PathToPeptideFile, readProteinMapFromFasta(mth), 
                                new Map<string, ModificationTerminal>(termMods), 
                                new Map<string, ModificationResidual>(resMods))

        let reader =  createPeptideReader(pars) 

        /// Compares to amino acid sequences as arrays
        let isEqual (ar1:AminoAcids.AminoAcid[]) (ar2:AminoAcids.AminoAcid[]) =
            let rec loop index =
                if index < 0 then
                    true
                else
                    if (AminoAcids.isEqual ar1.[index] ar2.[index]) then
                        loop (index - 1)
                    else
                        false
            if ar1.Length = ar2.Length then
                loop (ar1.Length - 1 )
            else
                false

        /// Groups PeptideInfo to PeptideInfoGroup with distinct amino acid sequneces
        let groupToPeptideInfoGroup (input:list<float*PeptideInfo>) =
            let rec groupLoop (first:float*PeptideInfo) (heap:list<float*PeptideInfo>) (stack:list<float*PeptideInfo>) (groupStack:list<string*Digestion.CleavageIndex>) =
                match heap with 
                | head::rest -> let massF,peptideInfoF = first
                                let mass,peptideInfo = head
                                if (isEqual peptideInfoF.Sequence peptideInfo.Sequence) then                            
                                    groupLoop first rest stack ((peptideInfo.ProteinID,peptideInfo.CleavageIndex)::groupStack)
                                else
                                    groupLoop first rest (head::stack) groupStack
                        
                | []         -> let massF,peptideInfoF = first
                                stack,(createPeptideInfoGroup ((peptideInfoF.ProteinID,peptideInfoF.CleavageIndex)::groupStack) peptideInfoF.Sequence massF)
    



            let rec outerLoop (heap:list<float*PeptideInfo>) (stack:list<PeptideInfoGroup>) =
                match heap with 
                | head::rest -> let filteredStack,groupStack = groupLoop head rest [] []
                                outerLoop filteredStack (groupStack::stack)
                | []         -> stack

    

            outerLoop input []


        member private this.Prune() = 
            if (cache.Count > cacheSize)  then          
                let key = cache.Keys.First()            
                cache.Remove(key) |> ignore
                    
        member private this.Get(mk:MassIndexKey, reader:PeptideReader) =
            if cache.ContainsKey(mk.PeptidePointer) then
                cache.[mk.PeptidePointer]
            else
                this.Prune()
                let pep = reader.ReadPeptide(mk.PeptidePointer)
                cache.Add(mk.PeptidePointer, pep)
                pep

        member this.FindPeptides(lowMass:float, highMass:float) =
            if massIndex = null then
                let idxPath = pars.PathToIndexFile    
                if File.Exists(idxPath) = false then raise (System.IO.IOException("Index file not found: " + idxPath))        
                massIndex <- PeptideMassIndex.Open(idxPath) 

            let massKeys = massIndex.Search(lowMass, highMass)  
                                               
            Seq.toList (seq {for mk in massKeys do yield (mk.MassKey, this.Get(mk, reader)) } )

        member this.FindLabeledPeptides(lowMass:float, highMass:float) =     
            if labeledMassIndex = null then                         
                let idxPath = pars.PathToLabeledIndexFile    
                if File.Exists(idxPath) = false then raise (System.IO.IOException("Index file not found: " + idxPath))       
                labeledMassIndex <- PeptideMassIndex.Open(idxPath) 

            let label = IsotopicLabels.Table.SymbolAsObject pars.IsotopicLabelName
            let massKeys = labeledMassIndex.Search(lowMass, highMass)  
                      
            Seq.toList (seq {
                for mk in massKeys do 
                    let pep = this.Get(mk, reader)
                    let labelSeq = PeptideDBBuilder.labelFun(pep.Sequence, label)
                    let labeledPep = { pep with Sequence=labelSeq }
                    yield (mk.MassKey, labeledPep)
            }) 

        member this.getPeptidesByPpm (ppm:float) (mass:float) =
            let delta = Mass.deltaMassByPpm ppm mass
            // List is not amino acid sequence distinct
            this.FindPeptides((mass - delta),(mass + delta))            

        member this.getGroupedPeptidesByPpm (ppm:float) (mass:float) =
            let delta = Mass.deltaMassByPpm ppm mass            
            this.FindPeptides((mass - delta),(mass + delta)) |> groupToPeptideInfoGroup
                    
        member this.getPeptidesByPpmWithLabel (labelFun:seq<AminoAcid> -> seq<AminoAcid>) (ppm:float) (mass:float) =
            let delta = Mass.deltaMassByPpm ppm mass
            // List is not amino acid sequence distinct
            this.FindLabeledPeptides((mass - delta),(mass + delta))            

        member this.getGroupedPeptidesByPpmWithLabel (ppm:float) (mass:float) =
            let delta = Mass.deltaMassByPpm ppm mass            
            this.FindLabeledPeptides((mass - delta),(mass + delta)) |> groupToPeptideInfoGroup 
        
        member this.Close() =      
            if massIndex <> null then      
                massIndex.Shutdown()
                massIndex <- null

            if labeledMassIndex <> null then      
                labeledMassIndex.Shutdown()
                labeledMassIndex <- null

            reader.Close()
            cache.Clear()

        interface System.IDisposable with 
            member this.Dispose() = this.Close()

    end

//type MassIndexKey 
//    ( massKey        : float,
//      peptidePointer : int64
//    ) = 
//    member this.MassKey        = massKey
//    member this.PeptidePointer = peptidePointer
//        
//    override this.ToString() =
//        sprintf "MassIndexEntry: MassKey=%f; PeptidePointer=%A" this.MassKey this.PeptidePointer





