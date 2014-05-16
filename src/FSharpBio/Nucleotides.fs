namespace FSharpBio

module Nucleotides =

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
    let CodonMap = [((U,U,U), AminoAcids.Phe);
                    ((U,U,C), AminoAcids.Phe);
                    ((U,U,A), AminoAcids.Leu);
                    ((U,U,G), AminoAcids.Leu);

                    ((U,C,U), AminoAcids.Ser);
                    ((U,C,C), AminoAcids.Ser);
                    ((U,C,A), AminoAcids.Ser);
                    ((U,C,G), AminoAcids.Ser);

                    ((U,A,U), AminoAcids.Tyr);
                    ((U,A,C), AminoAcids.Tyr);
                    ((U,A,A), AminoAcids.Ter);
                    ((U,A,G), AminoAcids.Ter);

                    ((U,G,U), AminoAcids.Cys);
                    ((U,G,C), AminoAcids.Cys);
                    ((U,G,A), AminoAcids.Ter);
                    ((U,G,G), AminoAcids.Trp);

                    ((C,U,U), AminoAcids.Leu);
                    ((C,U,C), AminoAcids.Leu);
                    ((C,U,A), AminoAcids.Leu);
                    ((C,U,G), AminoAcids.Leu);

                    ((C,C,U), AminoAcids.Pro);
                    ((C,C,C), AminoAcids.Pro);
                    ((C,C,A), AminoAcids.Pro);
                    ((C,C,G), AminoAcids.Pro);

                    ((C,A,U), AminoAcids.His);
                    ((C,A,C), AminoAcids.His);
                    ((C,A,A), AminoAcids.Gln);
                    ((C,A,G), AminoAcids.Gln);

                    ((C,G,U), AminoAcids.Arg);
                    ((C,G,C), AminoAcids.Arg);
                    ((C,G,A), AminoAcids.Arg);
                    ((C,G,G), AminoAcids.Arg);

                    ((A,U,U), AminoAcids.Ile);
                    ((A,U,C), AminoAcids.Ile);
                    ((A,U,A), AminoAcids.Ile);
                    ((A,U,G), AminoAcids.Met);

                    ((A,C,U), AminoAcids.Thr);
                    ((A,C,C), AminoAcids.Thr);
                    ((A,C,A), AminoAcids.Thr);
                    ((A,C,G), AminoAcids.Thr);

                    ((A,A,U), AminoAcids.Asn);
                    ((A,A,C), AminoAcids.Asn);
                    ((A,A,A), AminoAcids.Lys);
                    ((A,A,G), AminoAcids.Lys);

                    ((A,G,U), AminoAcids.Ser);
                    ((A,G,C), AminoAcids.Ser);
                    ((A,G,A), AminoAcids.Arg);
                    ((A,G,G), AminoAcids.Arg);

                    ((G,U,U), AminoAcids.Val);
                    ((G,U,C), AminoAcids.Val);
                    ((G,U,A), AminoAcids.Val);
                    ((G,U,G), AminoAcids.Val);

                    ((G,C,U), AminoAcids.Ala);
                    ((G,C,C), AminoAcids.Ala);
                    ((G,C,A), AminoAcids.Ala);
                    ((G,C,G), AminoAcids.Ala);

                    ((G,A,U), AminoAcids.Asp);
                    ((G,A,C), AminoAcids.Asp);
                    ((G,A,A), AminoAcids.Glu);
                    ((G,A,G), AminoAcids.Glu);

                    ((G,G,U), AminoAcids.Gly);
                    ((G,G,C), AminoAcids.Gly);
                    ((G,G,A), AminoAcids.Gly);
                    ((G,G,G), AminoAcids.Gly);
                    ((Gap,Gap,Gap), AminoAcids.Gap);] |> Map.ofSeq


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
