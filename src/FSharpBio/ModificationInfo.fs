namespace Fsharp.Bio

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
    
    type ModificationInfo =
        | Residual of ModificationResidual
        | Terminal of ModificationTerminal        
        | Both     of ModificationResidual * ModificationTerminal         

    /// Returns true if AminoAcid is modified at the residual
    let isResidualModified (m:ModificationInfo) =
        match m with
        | ModificationInfo.Residual (_) -> true
        | ModificationInfo.Both (_)     -> true
        | _                             -> false   


    /// Returns true if AminoAcid is modified at the terminal
    let isTerminalModified (m:ModificationInfo) =
        match m with
        | ModificationInfo.Terminal (_) -> true
        | ModificationInfo.Both (_)     -> true
        | _                             -> false


    let formula (m:ModificationInfo) =
        match m with
        | ModificationInfo.Residual (mr) -> mr.Formula
        | ModificationInfo.Terminal (mt) -> mt.Formula
        | ModificationInfo.Both (mr,mt)  -> Formula.add mr.Formula mt.Formula
                                                    
