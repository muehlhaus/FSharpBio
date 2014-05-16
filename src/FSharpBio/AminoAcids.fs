namespace FSharpBio

module AminoAcids =

    /// Amino acid Codes
    type AminoAcidLiteral =
    /// A  *Alanin
    | Ala        
    /// C  *Cysteine
    | Cys
    /// D  *Aspartic Acid
    | Asp
    /// E  *Glutamic Acid
    | Glu 
    /// F  *Glutamic Acid
    | Phe
    /// G  *Glycine
    | Gly
    /// H  *Histidine
    | His
    /// I  *Isoleucine
    | Ile
    /// K  *Lysine
    | Lys
    /// L  *Leucine
    | Leu
    /// M  *Methionine
    | Met
    /// N  *Asparagine
    | Asn
    /// O  *Pyrrolysine
    | Pyl
    /// P  *Proline
    | Pro        
    /// Q  *Glutamine
    | Gln
    /// R  *Arginine
    | Arg
    /// S  *Serine
    | Ser        
    /// T  *Threonine
    | Thr
    /// U  *Selenocysteine
    | Sel
    /// V  *Valine
    | Val
    /// W  *Tryptophan
    | Trp
    /// Y  *Tyrosine
    | Tyr

    /// X  *Unspecified
    | Xaa        
    /// J  *Leucine/Isoleucine
    | Xle
    /// Z  *Glutamine/glutamic acid
    | Glx
    /// B  *Asparagine/aspartic acid
    | Asx

    /// -  *Gap 
    | Gap
    /// *  *Termination
    | Ter

    
    type ParsedAminoAcidChar = 
        | StandardCodes  of AminoAcidLiteral
        | AmbiguityCodes of AminoAcidLiteral
        | BadChar        of char


    let charToParsedAminoAcidChar (c:char) =
        match System.Char.ToUpper c with                                    
        | 'A' ->  StandardCodes AminoAcidLiteral.Ala            
        | 'C' ->  StandardCodes AminoAcidLiteral.Cys
        | 'D' ->  StandardCodes AminoAcidLiteral.Asp
        | 'E' ->  StandardCodes AminoAcidLiteral.Glu
        | 'F' ->  StandardCodes AminoAcidLiteral.Phe            
        | 'G' ->  StandardCodes AminoAcidLiteral.Gly
        | 'H' ->  StandardCodes AminoAcidLiteral.His
        | 'I' ->  StandardCodes AminoAcidLiteral.Ile            
        | 'K' ->  StandardCodes AminoAcidLiteral.Lys
        | 'L' ->  StandardCodes AminoAcidLiteral.Leu            
        | 'M' ->  StandardCodes AminoAcidLiteral.Met
        | 'N' ->  StandardCodes AminoAcidLiteral.Asn            
        | 'P' ->  StandardCodes AminoAcidLiteral.Pro
        | 'Q' ->  StandardCodes AminoAcidLiteral.Gln
        | 'R' ->  StandardCodes AminoAcidLiteral.Arg            
        | 'S' ->  StandardCodes AminoAcidLiteral.Ser
        | 'T' ->  StandardCodes AminoAcidLiteral.Thr            
        | 'V' ->  StandardCodes AminoAcidLiteral.Val
        | 'W' ->  StandardCodes AminoAcidLiteral.Trp
        | 'Y' ->  StandardCodes AminoAcidLiteral.Tyr
        // special amino acids
        | 'O' ->  StandardCodes AminoAcidLiteral.Pyl
        | 'U' ->  StandardCodes AminoAcidLiteral.Sel
        // ambiguis amino acids
        | 'X' ->  AmbiguityCodes AminoAcidLiteral.Xaa            
        | 'Z' ->  AmbiguityCodes AminoAcidLiteral.Glx
        | 'B' ->  AmbiguityCodes AminoAcidLiteral.Asx
        | 'J' ->  AmbiguityCodes AminoAcidLiteral.Xle
        // termination and gap
        | '-' ->  StandardCodes AminoAcidLiteral.Gap
        | '*' ->  StandardCodes AminoAcidLiteral.Ter            
        // bad character
        | ch -> BadChar ch


    module Propteries =
        
         

        //Amino acid formulas minus H20   
        let formula (aa:AminoAcidLiteral) =
            match aa with
            | AminoAcidLiteral.Ala -> (Formula.parseFormulaString "C3H5ON"    )
            | AminoAcidLiteral.Cys -> (Formula.parseFormulaString "C3H5ONS"   )
            | AminoAcidLiteral.Asp -> (Formula.parseFormulaString "C4H5O3N"   )
            | AminoAcidLiteral.Glu -> (Formula.parseFormulaString "C5H7O3N"   )
            | AminoAcidLiteral.Phe -> (Formula.parseFormulaString "C9H9ON"    )
            | AminoAcidLiteral.Gly -> (Formula.parseFormulaString "C2H3ON"    )
            | AminoAcidLiteral.His -> (Formula.parseFormulaString "C6H7ON3"   )
            | AminoAcidLiteral.Ile -> (Formula.parseFormulaString "C6H11ON"   )
            | AminoAcidLiteral.Lys -> (Formula.parseFormulaString "C6H12ON2"  )
            | AminoAcidLiteral.Leu -> (Formula.parseFormulaString "C6H11ON"   )
            | AminoAcidLiteral.Met -> (Formula.parseFormulaString "C5H9ONS"   )
            | AminoAcidLiteral.Asn -> (Formula.parseFormulaString "C4H6O2N2"  )
            | AminoAcidLiteral.Pyl -> (Formula.parseFormulaString "C12H19N3O2") // Pyrrolysine
            | AminoAcidLiteral.Pro -> (Formula.parseFormulaString "C5H7ON"    )
            | AminoAcidLiteral.Gln -> (Formula.parseFormulaString "C5H8O2N2"  )
            | AminoAcidLiteral.Arg -> (Formula.parseFormulaString "C6H12ON4"  )
            | AminoAcidLiteral.Ser -> (Formula.parseFormulaString "C3H5O2N"   )
            | AminoAcidLiteral.Thr -> (Formula.parseFormulaString "C4H7O2N"   )
            | AminoAcidLiteral.Sel -> (Formula.parseFormulaString "C3H5NOSe"  ) // Selenocysteine
            | AminoAcidLiteral.Val -> (Formula.parseFormulaString "C5H9ON"    )
            | AminoAcidLiteral.Trp -> (Formula.parseFormulaString "C11H10ON2" )
            | AminoAcidLiteral.Tyr -> (Formula.parseFormulaString "C9H9O2N"   )
  
            | AminoAcidLiteral.Xaa -> (Formula.parseFormulaString "C2H3N1O1"  )
            | AminoAcidLiteral.Xle -> (Formula.parseFormulaString "C6H11N1O1" )
            | AminoAcidLiteral.Glx -> (Formula.parseFormulaString "C5H7N1O3"  )
            | AminoAcidLiteral.Asx -> (Formula.parseFormulaString "C4H5N1O3"  )
  
            | AminoAcidLiteral.Gap -> (Formula.emptyFormula)
            | AminoAcidLiteral.Ter -> (Formula.emptyFormula)


        let name (aa:AminoAcidLiteral) =
            match aa with
            | AminoAcidLiteral.Ala -> "Alanin"          
            | AminoAcidLiteral.Cys -> "Cysteine"       
            | AminoAcidLiteral.Asp -> "Aspartic Acid"  
            | AminoAcidLiteral.Glu -> "Glutamic Acid"  
            | AminoAcidLiteral.Phe -> "Phenylalanin"   
            | AminoAcidLiteral.Gly -> "Glycine"        
            | AminoAcidLiteral.His -> "Histidine"      
            | AminoAcidLiteral.Ile -> "Isoleucine"     
            | AminoAcidLiteral.Lys -> "Lysine"         
            | AminoAcidLiteral.Leu -> "Leucine"        
            | AminoAcidLiteral.Met -> "Methionine"     
            | AminoAcidLiteral.Asn -> "Asparagine"     
            | AminoAcidLiteral.Pyl -> "Pyrrolysine"    
            | AminoAcidLiteral.Pro -> "Proline"        
            | AminoAcidLiteral.Gln -> "Glutamine"      
            | AminoAcidLiteral.Arg -> "Arginine"       
            | AminoAcidLiteral.Ser -> "Serine"         
            | AminoAcidLiteral.Thr -> "Threonine"      
            | AminoAcidLiteral.Sel -> "Selenocysteine" 
            | AminoAcidLiteral.Val -> "Valine"         
            | AminoAcidLiteral.Trp -> "Tryptophan"     
            | AminoAcidLiteral.Tyr -> "Tyrosine"       
  
            | AminoAcidLiteral.Xaa -> "Unspecified"             
            | AminoAcidLiteral.Xle -> "Leucine/Isoleucine"      
            | AminoAcidLiteral.Glx -> "Glutamine/glutamic acid" 
            | AminoAcidLiteral.Asx -> "Asparagine/aspartic acid"
  
            | AminoAcidLiteral.Gap -> "Gap"
            | AminoAcidLiteral.Ter -> "Ter"

        let symbol (aa:AminoAcidLiteral) =
            match aa with
            | AminoAcidLiteral.Ala -> 'A' 
            | AminoAcidLiteral.Cys -> 'C'
            | AminoAcidLiteral.Asp -> 'D'
            | AminoAcidLiteral.Glu -> 'E'
            | AminoAcidLiteral.Phe -> 'F'
            | AminoAcidLiteral.Gly -> 'G'
            | AminoAcidLiteral.His -> 'H'
            | AminoAcidLiteral.Ile -> 'I'
            | AminoAcidLiteral.Lys -> 'K'
            | AminoAcidLiteral.Leu -> 'L'
            | AminoAcidLiteral.Met -> 'M'
            | AminoAcidLiteral.Asn -> 'N'
            | AminoAcidLiteral.Pyl -> 'O'
            | AminoAcidLiteral.Pro -> 'P'
            | AminoAcidLiteral.Gln -> 'Q'
            | AminoAcidLiteral.Arg -> 'R'
            | AminoAcidLiteral.Ser -> 'S'
            | AminoAcidLiteral.Thr -> 'T'
            | AminoAcidLiteral.Sel -> 'U'
            | AminoAcidLiteral.Val -> 'V'
            | AminoAcidLiteral.Trp -> 'W'
            | AminoAcidLiteral.Tyr -> 'Y'
  
            | AminoAcidLiteral.Xaa -> 'X'
            | AminoAcidLiteral.Xle -> 'J'
            | AminoAcidLiteral.Glx -> 'Z'
            | AminoAcidLiteral.Asx -> 'B'
  
            | AminoAcidLiteral.Gap -> '-'
            | AminoAcidLiteral.Ter -> '*'

        let hydrophobicity (aa:AminoAcidLiteral) =
            match aa with
            | AminoAcidLiteral.Ala -> 1.0          
            | AminoAcidLiteral.Cys -> 1.0 
            | AminoAcidLiteral.Asp -> 1.0 
            | AminoAcidLiteral.Glu -> 1.0  
            | AminoAcidLiteral.Phe -> 1.0 
            | AminoAcidLiteral.Gly -> 1.0 
            | AminoAcidLiteral.His -> 1.0 
            | AminoAcidLiteral.Ile -> 1.0 
            | AminoAcidLiteral.Lys -> 1.0 
            | AminoAcidLiteral.Leu -> 1.0 
            | AminoAcidLiteral.Met -> 1.0 
            | AminoAcidLiteral.Asn -> 1.0 
            | AminoAcidLiteral.Pyl -> 1.0 
            | AminoAcidLiteral.Pro -> 1.0         
            | AminoAcidLiteral.Gln -> 1.0 
            | AminoAcidLiteral.Arg -> 1.0 
            | AminoAcidLiteral.Ser -> 1.0         
            | AminoAcidLiteral.Thr -> 1.0 
            | AminoAcidLiteral.Sel -> 1.0 
            | AminoAcidLiteral.Val -> 1.0 
            | AminoAcidLiteral.Trp -> 1.0 
            | AminoAcidLiteral.Tyr -> 1.0 
  
            | AminoAcidLiteral.Xaa -> 1.0         
            | AminoAcidLiteral.Xle -> 1.0 
            | AminoAcidLiteral.Glx -> 1.0 
            | AminoAcidLiteral.Asx -> 1.0 
  
            | AminoAcidLiteral.Gap -> nan 
            | AminoAcidLiteral.Ter -> nan 
