namespace FSharpBio

module NucleotideLiteral =

    /// Nucleotide Codes
    type NucleotideLiteral =
    // ´Standard Nucleotide Codes
    /// A : Adenine
    | A
    /// T : Thymidine (only DNA)
    | T
    /// G : Guanine
    | G
    /// C : Cytosine
    | C
    /// U : Uracil    (only RNA)
    | U
    /// I : Inosine   (only RNA)
    | I
    /// - : Gap
    | Gap
    /// * : Terminator
    | Ter
        
    // 'Ambiguous Nucleotide Codes: double base codes
    /// R : G or A = puRine
    | R
    /// Y : U/T or C = pYrimidine
    | Y
    /// K : G or U = Keto
    | K
    /// M : A or C = aMino
    | M
    /// S : G or C = Strong base pair
    | S
    /// W : A or U = Weak base pair 
    | W
        
    // 'Ambiguous Nucleotide Codes: triple base codes
    /// B : G or U or C = not A
    | B
    /// D : G or A or U = not C
    | D
    /// H : A or C or U = not G
    | H
    /// V : G or V or A = not T/U
    | V

    // 'Ambiguous Nucleotide Codes
    /// N : A or G or U or C.
    | N


    type ParsedNucleotideChar = 
        | StandardCodes    of NucleotideLiteral
        | Standard_DNAonly of NucleotideLiteral
        | Standard_RNAonly of NucleotideLiteral
        | AmbiguityCodes   of NucleotideLiteral                
        | BadChar          of char


    let charToParsedNucleotideChar (c:char) =
        match System.Char.ToUpper c with                                    
        
        | 'A' -> StandardCodes    NucleotideLiteral.A
        | 'T' -> Standard_DNAonly NucleotideLiteral.T
        | 'G' -> StandardCodes    NucleotideLiteral.G
        | 'C' -> StandardCodes    NucleotideLiteral.C
            
        | 'U' -> Standard_RNAonly NucleotideLiteral.U
        | 'I' -> Standard_RNAonly NucleotideLiteral.I
        // termination and gap
        | '-' -> StandardCodes    NucleotideLiteral.Gap
        | '*' -> StandardCodes    NucleotideLiteral.Ter

        | 'R' -> AmbiguityCodes NucleotideLiteral.R
        | 'Y' -> AmbiguityCodes NucleotideLiteral.Y
        | 'K' -> AmbiguityCodes NucleotideLiteral.K
        | 'M' -> AmbiguityCodes NucleotideLiteral.M
        | 'S' -> AmbiguityCodes NucleotideLiteral.S
        | 'W' -> AmbiguityCodes NucleotideLiteral.W

        | 'B' -> AmbiguityCodes NucleotideLiteral.B
        | 'D' -> AmbiguityCodes NucleotideLiteral.D
        | 'H' -> AmbiguityCodes NucleotideLiteral.H
        | 'V' -> AmbiguityCodes NucleotideLiteral.V

        | 'N' -> AmbiguityCodes NucleotideLiteral.N        
        // bad character
        | ch -> BadChar ch
        
        
        
    /// Create the complement DNA or RNA strand. For example, the sequence "ATGC" is converted to "TACG"
    let complement (nuc:NucleotideLiteral) =
        match nuc with
        | A    -> T 
        | T    -> A  
        | G    -> C
        | C    -> G
        | U    -> A                
        | R    -> Y
        | Y    -> R
        | K    -> M
        | M    -> K
        | B    -> V
        | D    -> H
        | H    -> D
        | V    -> B
        | _    -> nuc

        
    /// Create the inverse DNA or RNA strand. For example, the sequence "ATGC" is converted to "CGTA"
    let inverse (nuc:NucleotideLiteral) =           
        match nuc with
        | A -> C
        | T -> G
        | G -> T 
        | C -> A
            
        | U -> A        
                        
        // 'Ambiguous Nucleotide Codes: double base codes
        | R -> W
        | Y -> S
        | K -> M
        | M -> K
        | S -> Y
        | W -> R
        // 'Ambiguous Nucleotide Codes: triple base codes
        | B -> V
        | D -> H
        | H -> D
        | V -> B
        
        | _    -> nuc

    /// Create the antiparallel DNA or RNA strand. For example, the sequence "ATGC" is converted to "GCAT". "Antiparallel" combines the two functions "Complement" and "Inverse".
    let antiparallel (nuc:NucleotideLiteral) = 
        inverse (complement nuc)
            
        
    /// Replace thymidine (T) by uracil (U). For example, the sequence "ATUGC" is converted to "AUUGC".
    let replaceTbyU (nuc:NucleotideLiteral) =          
        match nuc with
        | T -> U
        | _ -> nuc


    /// Replace uracil (U) by thymidine (T). For example, the sequence "ATUGC" is converted to "ATTGC".
    let replaceUbyT (nuc:NucleotideLiteral) =
        match nuc with
        | U -> T
        | _ -> nuc



    /// Codon to AminoAcid 
    let CodonMap = [((U,U,U), AminoAcidLiteral.Phe);
                    ((U,U,C), AminoAcidLiteral.Phe);
                    ((U,U,A), AminoAcidLiteral.Leu);
                    ((U,U,G), AminoAcidLiteral.Leu);

                    ((U,C,U), AminoAcidLiteral.Ser);
                    ((U,C,C), AminoAcidLiteral.Ser);
                    ((U,C,A), AminoAcidLiteral.Ser);
                    ((U,C,G), AminoAcidLiteral.Ser);

                    ((U,A,U), AminoAcidLiteral.Tyr);
                    ((U,A,C), AminoAcidLiteral.Tyr);
                    ((U,A,A), AminoAcidLiteral.Ter);
                    ((U,A,G), AminoAcidLiteral.Ter);

                    ((U,G,U), AminoAcidLiteral.Cys);
                    ((U,G,C), AminoAcidLiteral.Cys);
                    ((U,G,A), AminoAcidLiteral.Ter);
                    ((U,G,G), AminoAcidLiteral.Trp);

                    ((C,U,U), AminoAcidLiteral.Leu);
                    ((C,U,C), AminoAcidLiteral.Leu);
                    ((C,U,A), AminoAcidLiteral.Leu);
                    ((C,U,G), AminoAcidLiteral.Leu);

                    ((C,C,U), AminoAcidLiteral.Pro);
                    ((C,C,C), AminoAcidLiteral.Pro);
                    ((C,C,A), AminoAcidLiteral.Pro);
                    ((C,C,G), AminoAcidLiteral.Pro);

                    ((C,A,U), AminoAcidLiteral.His);
                    ((C,A,C), AminoAcidLiteral.His);
                    ((C,A,A), AminoAcidLiteral.Gln);
                    ((C,A,G), AminoAcidLiteral.Gln);

                    ((C,G,U), AminoAcidLiteral.Arg);
                    ((C,G,C), AminoAcidLiteral.Arg);
                    ((C,G,A), AminoAcidLiteral.Arg);
                    ((C,G,G), AminoAcidLiteral.Arg);

                    ((A,U,U), AminoAcidLiteral.Ile);
                    ((A,U,C), AminoAcidLiteral.Ile);
                    ((A,U,A), AminoAcidLiteral.Ile);
                    ((A,U,G), AminoAcidLiteral.Met);

                    ((A,C,U), AminoAcidLiteral.Thr);
                    ((A,C,C), AminoAcidLiteral.Thr);
                    ((A,C,A), AminoAcidLiteral.Thr);
                    ((A,C,G), AminoAcidLiteral.Thr);

                    ((A,A,U), AminoAcidLiteral.Asn);
                    ((A,A,C), AminoAcidLiteral.Asn);
                    ((A,A,A), AminoAcidLiteral.Lys);
                    ((A,A,G), AminoAcidLiteral.Lys);

                    ((A,G,U), AminoAcidLiteral.Ser);
                    ((A,G,C), AminoAcidLiteral.Ser);
                    ((A,G,A), AminoAcidLiteral.Arg);
                    ((A,G,G), AminoAcidLiteral.Arg);

                    ((G,U,U), AminoAcidLiteral.Val);
                    ((G,U,C), AminoAcidLiteral.Val);
                    ((G,U,A), AminoAcidLiteral.Val);
                    ((G,U,G), AminoAcidLiteral.Val);

                    ((G,C,U), AminoAcidLiteral.Ala);
                    ((G,C,C), AminoAcidLiteral.Ala);
                    ((G,C,A), AminoAcidLiteral.Ala);
                    ((G,C,G), AminoAcidLiteral.Ala);

                    ((G,A,U), AminoAcidLiteral.Asp);
                    ((G,A,C), AminoAcidLiteral.Asp);
                    ((G,A,A), AminoAcidLiteral.Glu);
                    ((G,A,G), AminoAcidLiteral.Glu);

                    ((G,G,U), AminoAcidLiteral.Gly);
                    ((G,G,C), AminoAcidLiteral.Gly);
                    ((G,G,A), AminoAcidLiteral.Gly);
                    ((G,G,G), AminoAcidLiteral.Gly); ] |> Map.ofSeq
                    


    /// <summary>
    /// Lookup an amino acid based on a triplet of nucleotides. U U U for instance
    /// will result in Phenylalanine.  If the values cannot be
    /// found in the lookup table, <c>false</c> will be returned.
    /// </summary>
    /// <param name="n1">The first character.</param>
    /// <param name="n2">The second character.</param>
    /// <param name="n3">The third character.</param>
    /// <returns>True/False if the value exists</returns>
    let lookupBytes (n1 : NucleotideLiteral, n2 : NucleotideLiteral, n3 : NucleotideLiteral ) =
        
        // TODO: 
        let codon = (n1,n2,n3)
        CodonMap.Item(codon)


    /// Properties of a nucleatide like formula, name, symbole, but also Physicochemical features
    module Properties = 
        
        //Nucleotide formulas minus H20   
        let formula (nuc:NucleotideLiteral) =
            match nuc with
            // ´Standard Nucleotide Codes
            /// A : Adenine
            | A   -> (Formula.parseFormulaString "C10H13N5O4")
            /// T : Thymidine (only DNA)
            | T   -> (Formula.parseFormulaString "C10H14N2O5")
            /// G : Guanine
            | G   -> (Formula.parseFormulaString "C10H13N5O5")
            /// C : Cytosine
            | C   -> (Formula.parseFormulaString "C9H13N3O5") 
            /// U : Uracil    (only RNA)
            | U   -> (Formula.parseFormulaString "C4H4N2O2")
            /// I : Inosine   (only RNA)
            | I   -> (Formula.parseFormulaString "C10H12N4O5")
            /// - : Gap
            | Gap -> (Formula.emptyFormula)
            /// * : Terminator
            | Ter -> (Formula.emptyFormula)
        
            // 'Ambiguous Nucleotide Codes: double base codes
            /// R : G or A = puRine
            | R   -> (Formula.emptyFormula)
            /// Y : U/T or C = pYrimidine
            | Y   -> (Formula.emptyFormula)
            /// K : G or U = Keto
            | K   -> (Formula.emptyFormula)
            /// M : A or C = aMino
            | M   -> (Formula.emptyFormula)
            /// S : G or C = Strong base pair
            | S   -> (Formula.emptyFormula)
            /// W : A or U = Weak base pair 
            | W   -> (Formula.emptyFormula)
        
            // 'Ambiguous Nucleotide Codes: triple base codes
            /// B : G or U or C = not A
            | B   -> (Formula.emptyFormula)
            /// D : G or A or U = not C
            | D   -> (Formula.emptyFormula)
            /// H : A or C or U = not G
            | H   -> (Formula.emptyFormula)
            /// V : G or V or A = not T/U
            | V   -> (Formula.emptyFormula)

            // 'Ambiguous Nucleotide Codes
            /// N : A or G or U or C.
            | N   -> (Formula.emptyFormula)
            

        // Nucleotide names
        let name (nuc:NucleotideLiteral) =
            match nuc with
            // ´Standard Nucleotide Codes
            /// A : Adenine
            | A   -> "Adenine"
            /// T : Thymidine (only DNA)
            | T   -> "Thymidine"
            /// G : Guanine
            | G   -> "Guanine"
            /// C : Cytosine
            | C   -> "Cytosine"
            /// U : Uracil    (only RNA)
            | U   -> "Uracil"
            /// I : Inosine   (only RNA)
            | I   -> "Inosine"
            /// - : Gap
            | Gap -> "Gap"
            /// * : Terminator
            | Ter -> "Ter"
        
            // 'Ambiguous Nucleotide Codes: double base codes
            /// R : G or A = puRine
            | R   ->  "puRine"
            /// Y : U/T or C = pYrimidine
            | Y   ->  "pYrimidine"
            /// K : G or U = Keto
            | K   ->  "Keto"
            /// M : A or C = aMino
            | M   -> "aMino"
            /// S : G or C = Strong base pair
            | S   ->  "Strong base pair"
            /// W : A or U = Weak base pair 
            | W   ->  "Weak base pair"
        
            // 'Ambiguous Nucleotide Codes: triple base codes
            /// B : G or U or C = not A
            | B   ->  "not A"
            /// D : G or A or U = not C
            | D   ->  "not C"
            /// H : A or C or U = not G
            | H   ->  "not G"
            /// V : G or V or A = not T/U
            | V   ->  "not T/U"

            // 'Ambiguous Nucleotide Codes
            /// N : A or G or U or C.
            | N   ->  "Unspecified"



        //Nucleotide symbol (One letter code)
        let symbol (nuc:NucleotideLiteral) =
            match nuc with
            // ´Standard Nucleotide Codes
            /// A : Adenine
            | A   -> 'A'
            /// T : Thymidine (only DNA)
            | T   -> 'T'
            /// G : Guanine
            | G   -> 'G'
            /// C : Cytosine
            | C   -> 'C'
            /// U : Uracil    (only RNA)
            | U   -> 'U'
            /// I : Inosine   (only RNA)
            | I   -> 'I'
            /// - : Gap
            | Gap -> '-'
            /// * : Terminator
            | Ter -> '*'
        
            // 'Ambiguous Nucleotide Codes: double base codes
            /// R : G or A = puRine
            | R   -> 'R'
            /// Y : U/T or C = pYrimidine
            | Y   -> 'Y'
            /// K : G or U = Keto
            | K   -> 'K'
            /// M : A or C = aMino
            | M   -> 'M'
            /// S : G or C = Strong base pair
            | S   -> 'S'
            /// W : A or U = Weak base pair 
            | W   -> 'W'
        
            // 'Ambiguous Nucleotide Codes: triple base codes
            /// B : G or U or C = not A
            | B   -> 'B'
            /// D : G or A or U = not C
            | D   -> 'D'
            /// H : A or C or U = not G
            | H   -> 'H'
            /// V : G or V or A = not T/U
            | V   -> 'V'

            // 'Ambiguous Nucleotide Codes
            /// N : A or G or U or C.
            | N   -> 'N'
        
