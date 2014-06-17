namespace FSharpBio

open Microsoft.FSharp.Reflection 
open System.Reflection 
open System.Runtime.Serialization 

module ModificationInfo =

    [<StructuralEquality;StructuralComparison>]
    type ModificationResidual = { Name            : string;
                                  GeneralName     : string;
                                  Formula         : Formula.Formula;                                                        
                                  AminoAcidSymbol : char;
                                }


    /// Creates a residual Modification
    let createModificationResidual name generalName formula aminoAcidSymbol =
        { Name = name; GeneralName = generalName; Formula = formula; AminoAcidSymbol = aminoAcidSymbol; }

    
//    type ModTerminalLocation = Any = 0 | Cterm = 1 | Nterm = 2 | ProteinCterm = 3 | ProteinNterm = 4
    type ModTerminalLocation = Cterm = 1 | Nterm = 2 | ProteinCterm = 3 | ProteinNterm = 4

    [<StructuralEquality;StructuralComparison>]
    type ModificationTerminal = { Name            : string;
                                  GeneralName     : string;
                                  Formula         : Formula.Formula;                                                                                  
                                  AminoAcidSymbol : char;
                                  Location        : ModTerminalLocation;
                                }

    /// Creates a terminal Modification
    let createModificationTerminal name generalName formula aminoAcidSymbol location =
        { Name = name; GeneralName = generalName; Formula = formula; AminoAcidSymbol = aminoAcidSymbol; Location = location; }
    
    type Modification =
        | Residual of ModificationResidual
        | Terminal of ModificationTerminal        
        | Both     of ModificationResidual * ModificationTerminal         

    /// Returns true if AminoAcid is modified at the residual
    let isResidualModified (m:Modification) =
        match m with
        | Modification.Residual (_) -> true
        | Modification.Both (_)     -> true
        | _                             -> false   


    /// Returns true if AminoAcid is modified at the terminal
    let isTerminalModified (m:Modification) =
        match m with
        | Modification.Terminal (_) -> true
        | Modification.Both (_)     -> true
        | _                             -> false


    let formula (m:Modification) =
        match m with
        | Modification.Residual (mr) -> mr.Formula
        | Modification.Terminal (mt) -> mt.Formula
        | Modification.Both (mr,mt)  -> Formula.add mr.Formula mt.Formula
                                                    
