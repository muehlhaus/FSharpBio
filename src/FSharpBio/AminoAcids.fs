namespace FSharpBio

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AminoAcids =

    open AminoAcidLiteral

    /// Marker interface for generic AminoAcids
    type IAminoAcid =
        inherit IBioItem
        abstract member ToLiteral : AminoAcidLiteral


    /// <summary>
    ///   Record represents an amino acid               
    /// </summary>
    type AminoAcid =
        | Literal  of AminoAcidLiteral
        | Isotopic of AminoAcidLiteral * IsotopicLabels.IsotopicLabel
        | Modified of AminoAcidLiteral * ModificationInfo.Modification
        | Both     of AminoAcidLiteral * IsotopicLabels.IsotopicLabel * ModificationInfo.Modification
        static member private getLiteral (a:AminoAcid) =
            match a with
            | AminoAcid.Literal (a)    -> a
            | AminoAcid.Isotopic (a,i) -> a
            | AminoAcid.Modified (a,m) -> a
            | AminoAcid.Both (a,i,m)   -> a
        
        interface IAminoAcid with
            member this.ToLiteral = AminoAcid.getLiteral this
            member this.Name     = (AminoAcid.getLiteral this) |> Propteries.name 
            member this.Symbol   = (AminoAcid.getLiteral this) |> Propteries.symbol
            member this.ByteCode = (AminoAcid.getLiteral this) |> Propteries.symbol |> byte
            member this.Formula  = 
                match this with
                | AminoAcid.Literal (a)    -> a |> Propteries.formula
                | AminoAcid.Isotopic (a,i) -> a |> Propteries.formula |> i.Label     // label
                | AminoAcid.Modified (a,m) -> let af = a |> Propteries.formula
                                              let mf = m |> ModificationInfo.formula
                                              Formula.add af mf
                | AminoAcid.Both (a,i,m)   -> let af = a |> Propteries.formula
                                              let mf = m |> ModificationInfo.formula
                                              Formula.add af mf  |> i.Label          // label
            member this.isTerminator = match (AminoAcid.getLiteral this) with
                                       | AminoAcidLiteral.Ter -> true
                                       | _                     -> false
            member this.isGap        = match (AminoAcid.getLiteral this) with
                                       | AminoAcidLiteral.Gap -> true
                                       | _                     -> false
                

    /// Compares two AminoAcids and returns true if equal
    let isEqual (a:AminoAcid) (b:AminoAcid) =                 
        if obj.ReferenceEquals(a,b) then
            true
        else
            match a,b with
            | AminoAcid.Literal  (a),AminoAcid.Literal  (b)         -> false // reference equality already checked ergo: false                                                                       
            | AminoAcid.Isotopic (a,ai),AminoAcid.Isotopic (b,bi)   -> if a.Equals(b) then ai.Equals(bi) else false                                                                                     
            | AminoAcid.Modified (a,am),AminoAcid.Modified (b,bm)   -> if a.Equals(b) then am.Equals(bm) else false
            | AminoAcid.Both (a,ai,am),AminoAcid.Both (b,bi,bm)     -> if a.Equals(b) then am.Equals(bm) && ai.Equals(bi) else false
            | _,_                                                   -> false

    /// Returns the AminoAcidLiteral
    let toLiteral (a:AminoAcid) =
        match a with
        | AminoAcid.Literal (a)    -> a
        | AminoAcid.Isotopic (a,i) -> a
        | AminoAcid.Modified (a,m) -> a
        | AminoAcid.Both (a,i,m)   -> a

    /// Returns true if AminoAcid is modified
    let isModified (a:AminoAcid) = 
        match a with
        | AminoAcid.Literal (_)  -> false
        | AminoAcid.Isotopic (_) -> false
        | _                      -> true
        
    
    /// Returns true if AminoAcid is modified at the residual
    let isResidualModified (a:AminoAcid) =        
        match a with
        | AminoAcid.Literal (_)    -> false
        | AminoAcid.Isotopic (_)   -> false
        | AminoAcid.Modified (_,m) -> ModificationInfo.isResidualModified m
        | AminoAcid.Both (_,i,m)   -> ModificationInfo.isResidualModified m
        
    /// Returns true if AminoAcid is modified at the terminal
    let isTerminalModified (a:AminoAcid) =
        match a with
        | AminoAcid.Literal (_)    -> false
        | AminoAcid.Isotopic (_)   -> false
        | AminoAcid.Modified (_,m) -> ModificationInfo.isTerminalModified m
        | AminoAcid.Both (_,i,m)   -> ModificationInfo.isTerminalModified m

    /// Returns true if AminoAcid is isotobic labeled
    let isIsotopicLabeled (a:AminoAcid) =         
        match a with
        | AminoAcid.Literal (_)  -> false
        | AminoAcid.Modified (_) -> false
        | _                      -> true

              
    /// Creates an AminoAcid which is modified at the residual and terminus
    let setModifications (a:AminoAcid) modificationResidual terminalModification =
        let m = ModificationInfo.Both (modificationResidual,terminalModification)
        match a with
        | AminoAcid.Literal (a)    -> AminoAcid.Modified (a,m)
        | AminoAcid.Isotopic (a,i) -> AminoAcid.Both (a,i,m)
        | AminoAcid.Modified (_,m) -> raise (System.ArgumentException("Amino acid is allready modified")) 
        | AminoAcid.Both (_,i,m)   -> raise (System.ArgumentException("Amino acid is allready modified")) 
        
    
    /// Creates an AminoAcid which is residual modified
    let setResidualModification (a:AminoAcid) modification =
        match a with
        | AminoAcid.Literal  (a)    -> let m = ModificationInfo.Residual modification
                                       AminoAcid.Modified (a,m)
        | AminoAcid.Isotopic (a,i)  -> let m = ModificationInfo.Residual modification
                                       AminoAcid.Both (a,i,m) 
        | AminoAcid.Modified (a,m)  -> if (ModificationInfo.isResidualModified m) then raise (System.ArgumentException("Amino acid is allready modified"))
                                       else AminoAcid.Modified (a,m)
        | AminoAcid.Both (a,i,m)    -> if (ModificationInfo.isResidualModified m) then raise (System.ArgumentException("Amino acid is allready modified"))
                                       else AminoAcid.Both (a,i,m)
        
       
                           
       

    /// Returns amino acid formula minus H20 plus given Formula
    let formulaPlus (a:AminoAcid) (plus:Formula.Formula)=  
        match a with
        | AminoAcid.Literal (a)    -> Formula.add (Propteries.formula a) plus
        | AminoAcid.Isotopic (a,i) -> i.Label (Formula.add (Propteries.formula a) plus)
        | AminoAcid.Modified (a,m) -> Formula.add (Formula.add (Propteries.formula a) plus) (ModificationInfo.formula m) 
        | AminoAcid.Both (a,i,m)   -> i.Label (Formula.add (Formula.add (Propteries.formula a) plus) (ModificationInfo.formula m))
       

    /// Returns amino acid formula minus H20 minus given Formula
    let formulaMinus (a:AminoAcid) (minus:Formula.Formula)= 
        match a with
        | AminoAcid.Literal (a)    -> Formula.substract (Propteries.formula a) minus
        | AminoAcid.Isotopic (a,i) -> i.Label (Formula.substract (Propteries.formula a) minus)
        | AminoAcid.Modified (a,m) -> Formula.add (Formula.substract (Propteries.formula a) minus) (ModificationInfo.formula m) 
        | AminoAcid.Both (a,i,m)   -> i.Label (Formula.add (Formula.substract (Propteries.formula a) minus) (ModificationInfo.formula m))

        
    /// Returns isotopic label of amino acid as optional
    let isotopicLabel (a:AminoAcid) = 
        match a with
        | AminoAcid.Literal (a)    -> None
        | AminoAcid.Isotopic (a,i) -> Some i
        | AminoAcid.Modified (a,m) -> None
        | AminoAcid.Both (a,i,m)   -> Some i


    let isotopicLabelFunc (a:AminoAcid) (f:Formula.Formula)  = 
        match a with
        | AminoAcid.Literal (a)    -> f
        | AminoAcid.Isotopic (a,i) -> i.Label f
        | AminoAcid.Modified (a,m) -> f
        | AminoAcid.Both (a,i,m)   -> i.Label f

    /// Returns modificationInfo of amino acid as optional
    let modification (a:AminoAcid) = 
        match a with
        | AminoAcid.Literal (a)    -> None
        | AminoAcid.Isotopic (a,i) -> None
        | AminoAcid.Modified (a,m) -> Some m
        | AminoAcid.Both (a,i,m)   -> Some m                                

      

    /// Returns the symbol of AminoAcid
    let symbol (a:AminoAcid) =         
        Propteries.symbol (toLiteral a)

    
    /// Returns the symbol of AminoAcid
    let toChar (a:AminoAcid) =
        symbol a

    /// Returns the name of AminoAcid
    let name (a:AminoAcid) =
        Propteries.name (toLiteral a)



    /// Returns average mass of AminoAcid including H20    
    let averageMass (a:AminoAcid) =
        formulaPlus a Formula.Table.H2O        
        |> Formula.averageMass
        

    /// Returns monoisotopic mass of AminoAcid including H20
    let monoisoMass (a:AminoAcid) =
        formulaPlus a Formula.Table.H2O
        |> Formula.monoisoMass

            
            
            
            
                 