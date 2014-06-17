namespace FSharpBio

open System


module IsotopicLabels =
    
    [<Flags>]   
    type LabelAtoms =
          None = 0b000000000
        | C13  = 0b000000001
        | N15  = 0b000000010
        | O18  = 0b000000100
        | H2   = 0b000001000 // = 8
    
    
    [<CustomEquality; CustomComparison>]
    type IsotopicLabel =
        { Name        : string;
          Symbol      : string;
          LabelAtoms  : LabelAtoms;
          Label       : Formula.Formula -> Formula.Formula;
        }

        override x.Equals(yobj) =
            match yobj with
            | :? IsotopicLabel as y -> (x.Symbol = y.Symbol)
            | _ -> false

        override x.GetHashCode() = hash x.Symbol 
        interface System.IComparable with
          member x.CompareTo yobj =
              match yobj with
              | :? IsotopicLabel as y -> compare x.Symbol y.Symbol
              | _ -> invalidArg "yobj" "cannot compare values of different types"


    let create name symbol labelAtom labelF =
        { Name = name; Symbol = symbol; LabelAtoms = labelAtom; Label = labelF }
    
    // ##### ##### ##### ##### #####
    module Table = 
        
        let unlabeled = create "unlabeled" "#unlabeled" LabelAtoms.None (fun f -> f)
        let N15_Label = create "Full heavy nitrogen" "#N15" LabelAtoms.N15 (fun f -> Formula.lableElement f Elements.Table.N Elements.Table.Heavy.N15)
//        let C13_Label = create "Full heavy carbon" "#C13" LabelAtoms.C13 (fun f -> Formula.lableElement f Elements.Table.C Elements.Table.Heavy.C13)

        let SymbolAsObject (symbol:string) =
            match symbol with
            | "#unlabeled"   -> unlabeled
            | "#N15"         -> N15_Label            
            | _ -> raise (System.ArgumentException("Unknown isotopic label.", "symbol"))