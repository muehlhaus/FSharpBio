namespace FSharpBio

module AminoAcidLiteral =

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



//  ###############------------------------------------------------------------
    module Propteries =
        
        open FSharpBio.IO
        open FSharpBio.IO.SchemaReader
        open FSharpBio.IO.SchemaReader.Csv
        open FSharpBio.IO.SchemaReader.Attribute        
         
        type PropertyDescription = {
            Name:       string;
            Ala:         float;
            Arg:         float;
            Asn:         float;
            Asp:         float;
            Cys:         float;
            Gln:         float;
            Glu:         float;
            Gly:         float;
            His:         float;
            Ile:         float;
            Leu:         float;
            Lys:         float;
            Met:         float;
            Phe:         float;
            Pro:         float;
            Ser:         float;
            Thr:         float;
            Trp:         float;
            Tyr:         float;
            Val:         float;
            Sel:         float;
            Pyl:         float;
            Xaa:         float;
            Glx:         float;
            Asx:         float;
            Xle:         float;
            Author:     string; 
            Reference:  string;
            }
            
     
        let createPropertyDescription name ala arg asn asp cys gln glu gly his ile leu lys met phe pro ser thr trp tyr valine sel pyl xaa glx asx xle author reference =
            { Name = name; Ala = ala; Arg = arg; Asn = asn; Asp = asp; Cys = cys; Gln = gln; Glu = glu; Gly = gly; His = his; Ile = ile; Leu = leu; Lys = lys; Met = met; Phe = phe; 
                Pro =pro; Ser = ser; Thr = thr; Trp = trp; Tyr = tyr; Val = valine; Sel = sel; Pyl = pyl; Xaa = xaa; Glx = glx; Asx = asx; Xle = xle; Author = author; Reference = reference; }     
       
      
        let private propertyDescriptionMap = 
            [ {Name = "Optimized Matching Hydrophobicity"; Ala = -0.4; Arg = -0.59; Asn = -0.92; Asp = -1.31; Cys = 0.17; Gln = -0.91; Glu = -1.22; Gly = -0.67; His = -0.64; Ile = 1.25; 
                Leu = 1.22; Lys = -0.67; Met = 1.02; Phe = 1.92; Pro = -0.49; Ser = -0.55; Thr = -0.28; Trp = 0.5; Tyr = 1.67; Val = 0.91; Sel = nan; Pyl = nan; Xaa = nan; Glx = nan; Asx = nan; 
                Xle = nan; Author = "Sweet R.M., Eisenberg D"; Reference = "J. Mol. Biol. 171:479-488(1983)"};
              {Name = "Hydropathicity"; Ala = 1.8; Arg = -4.5; Asn = -3.5; Asp = -3.5; Cys = 2.5; Gln = -3.5; Glu = -3.5; Gly = -0.4; His = -3.2; Ile = 4.5; Leu = 3.8; Lys = -3.9; Met = 1.9;
                Phe = 2.8; Pro = -1.6; Ser = -0.8; Thr = -0.7; Trp = -0.9; Tyr = -1.3; Val = 4.2; Sel = nan; Pyl = nan; Xaa = nan; Glx = nan; Asx = nan; Xle = nan; Author = "Kyte J., Doolittle R.F"; 
                Reference = "J. Mol. Biol. 157:105-132(1982";  }; 
              {Name = "Hydrophobicity"; Ala = 0.44; Arg = -2.42; Asn = -1.32; Asp = -0.31; Cys = 0.58; Gln = -0.71; Glu = -0.34; Gly = 0.00; His = -0.01; Ile = 2.46; Leu = 2.46; Lys = -2.45;
                Met = 1.1; Phe = 2.54; Pro = 1.29; Ser = -0.84; Thr = -0.41; Trp = 2.56; Tyr = 1.63;
                Val = 1.73; Sel = nan; Pyl = nan; Xaa = nan; Glx = nan; Asx = nan; Xle = nan; Author ="Abraham D.J., Leo A.J."; Reference = "Proteins: Structure, Function and Genetics 2:130-152(1987)"; }
              {Name = "Molecular weight"; Ala = 89.00; Arg = 174.00; Asn = 132.00; Asp = 133.00; Cys = 121.00; Gln = 146.00; Glu = 147.00; Gly = 75.00; His = 155.00; Ile = 131.00; Leu = 131.00; Lys = 146.00; 
                Met = 149.00; Phe = 165.00; Pro = 115.00; Ser = 105.00; Thr = 119.00; Trp = 204.00; Tyr = 181.00; Val = 117.00; Sel = nan; Pyl = nan; Xaa = nan; Glx = nan; Asx = nan; Xle = nan; Author = "nan"; 
                Reference = "Most textbooks";  }; 
              {Name = "Bulkiness"; Ala = 11.50; Arg = 14.28; Asn = 12.82; Asp = 11.68; Cys = 13.46; Gln = 14.45; Glu = 13.57; Gly = 3.4; His = 13.69; Ile = 21.4; Leu = 21.4; Lys = 15.71; Met = 16.25; 
                Phe = 19.8; Pro = 17.43; Ser = 9.47; Thr = 15.77; Trp = 21.67; Tyr = 18.03; Val = 21.57; Sel = nan; Pyl = nan; Xaa = nan; Glx = nan; Asx = nan; Xle = nan; 
                Author = "Zimmerman J.M., Eliezer N., Simha R."; Reference = "J. Theor. Biol. 21:170-201(1968)";  };
              {Name = "Polarity (p)"; Ala = 8.10; Arg = 10.50; Asn = 11.60; Asp = 13.00; Cys = 5.50; Gln = 10.50; Glu = 12.30; Gly = 9.00; His = 10.40; Ile = 5.20; Leu = 4.90; Lys = 11.30; Met = 5.70; 
                Phe = 5.20; Pro = 8.00; Ser = 9.20; Thr = 8.60; Trp = 5.40; Tyr = 6.20; Val = 5.90; Sel = nan; Pyl = nan; Xaa = nan; Glx = nan; Asx = nan; Xle = nan; 
                Author = "Grantham R. "; Reference = "Science 185:862-864(1974)";  };
              {Name = "Recognition factors"; Ala = 78.00; Arg = 95.00; Asn = 94.00; Asp = 81.00; Cys = 89.00; Gln = 87.00; Glu = 78.00; Gly = 84.00; His = 84.00; Ile = 88.00; Leu = 85.00; Lys = 87.00; Met = 80.00; 
                Phe = 81.00; Pro = 91.00; Ser = 107.00; Thr = 93.00; Trp = 104.00; Tyr = 84.00; Val = 89.00; Sel = nan; Pyl = nan; Xaa = nan; Glx = nan; Asx = nan; Xle = nan; 
                Author = "Fraga S."; Reference = "Can. J. Chem. 60:2606-2610(1982)";  };     
              {Name = "Hydrophobicity (free energy of transfer to surface in kcal/mole)"; Ala = 0.61; Arg = 0.69; Asn = 0.89; Asp = 0.61; Cys = 0.36; Gln = 0.97; Glu = 0.51; Gly = 0.81; His = 0.69; 
                Ile = -1.45; Leu = -1.65; Lys = 0.46; Met = -0.66; Phe = -1.52; Pro = -0.17; Ser = 0.42; Thr = 0.29; Trp = -1.20; Tyr = -1.43; Val = -0.75; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Bull H.B., Breese K."; Reference = "Arch. Biochem. Biophys. 161:665-670(1974)";  };     
              {Name = "Hydrophobicity scale based on free energy of transfer (kcal/mole)"; Ala = 0.10; Arg = 1.91; Asn = 0.48; Asp = 0.78; Cys = -1.42; Gln = 0.95; Glu = 0.83; Gly = 0.33; His = -0.50; 
                Ile = -1.13; Leu = -1.18; Lys = 1.40; Met = -1.59; Phe = -2.12; Pro = 0.73; Ser = 0.52; Thr = 0.07; Trp = -0.51; Tyr = -0.21; Val = -1.27; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Guy H.R."; Reference = "Biophys J. 47:61-70(1985)";  };     
              {Name = "Hydrophobicity scale (contact energy derived from 3D data)"; Ala = 5.33; Arg = 4.18; Asn = 3.71; Asp = 3.59; Cys = 7.93; Gln = 3.87; Glu = 3.65; Gly = 4.48; His = 5.10; 
                Ile = 8.83; Leu = 8.47; Lys = 2.95; Met = 8.95; Phe = 9.03; Pro = 3.87; Ser = 4.09; Thr = 4.49; Trp = 7.66; Tyr = 5.89; Val = 7.63; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Miyazawa S., Jernigen R.L."; Reference = "Macromolecules 18:534-552(1985)";  };         
              {Name = "Hydrophobicity scale (pi-r)"; Ala = 0.39; Arg = -3.95; Asn = -1.91; Asp = -3.81; Cys = 0.25; Gln = -1.30; Glu = -2.91; Gly = 0.00; His = -0.64; 
                Ile = 1.82; Leu = 1.82; Lys = -2.77; Met = 0.96; Phe = 2.27; Pro = 0.99; Ser = -1.24; Thr = -1.00; Trp = 2.13; Tyr = 1.47; Val = 1.30; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Roseman M.A."; Reference = "J. Mol. Biol. 200:513-522(1988)";  };         
             {Name = "Hydration potential (kcal/mole) at 25øC"; Ala = 1.94; Arg = -19.92; Asn = -9.68; Asp = -10.95; Cys = -1.24; Gln = -9.38; Glu = -10.2; Gly = 2.39; His = -10.27; 
                Ile = 2.15; Leu = 2.28; Lys = -9.52; Met = -1.48; Phe = -0.76; Pro = 0.00; Ser = -5.06; Thr = -4.88; Trp = -5.88; Tyr = -6.11; Val = 1.99; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Wolfenden R.V., Andersson L., Cullis P.M., Southgate C.C.F."; Reference = "Biochemistry 20:849-855(1981)";  };
             {Name = "Hydrophobic constants derived from HPLC peptide retention times"; Ala = -0.30; Arg = -1.10; Asn = -0.20; Asp = -1.40; Cys = 6.30; Gln = -0.20; Glu = 0.00; Gly = 1.20; His = -1.30; 
                Ile = 4.30; Leu = 6.60; Lys = -3.60; Met = 2.50; Phe = 7.50; Pro = 2.20; Ser = -0.60; Thr = -2.20; Trp = 7.90; Tyr = 7.10; Val = 5.90; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Wilson K.J., Honegger A., Stotzel R.P., Hughes G.J."; Reference = "Biochem. J. 199:31-41(1981)";  }; 
             {Name = "Hydrophobicity indices at ph 3.4 determined by HPLC"; Ala = 0.42; Arg = -1.56; Asn = -1.03; Asp = -0.51; Cys = 0.84; Gln = -0.96; Glu = -0.37; Gly = 0.00; His = -2.28; 
                Ile = 1.81; Leu = 1.80; Lys = -2.28; Met = 1.18; Phe = 1.74; Pro = 0.86; Ser = -0.64; Thr = -0.26; Trp = 1.46; Tyr = 0.51; Val = 1.34; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Cowan R., Whittaker R.G. "; Reference = " Peptide Research 3:75-80(1990)";  };   
             {Name = "Mobilities of amino acids on chromatography paper (RF)"; Ala = 5.10; Arg = 2.00; Asn = 0.60; Asp = 0.70; Cys = 0.00; Gln = 1.40; Glu = 1.80; Gly = 4.10; His = 1.60; 
                Ile = 9.30; Leu = 10.00; Lys = 1.30; Met = 8.70; Phe = 9.60; Pro = 4.90; Ser = 3.10; Thr = 3.50; Trp = 9.20; Tyr = 8.20; Val = 8.50; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Aboderin A.A."; Reference = " Int. J. Biochem. 2:537-544(1971)";  };    
             {Name = "Retention coefficient in TFA"; Ala = 7.30; Arg = -3.60; Asn = -5.70; Asp = -2.90; Cys = -9.20; Gln = -0.30; Glu = -7.10; Gly = -1.20; His = -2.10; 
                Ile = 6.60; Leu = 20.00; Lys = -3.70; Met = 5.60; Phe = 19.20; Pro = 5.10; Ser = -4.10; Thr = 0.80; Trp = 16.30; Tyr = 5.90; Val = 3.50; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Browne C.A., Bennett H.P.J., Solomon S. "; Reference = "Anal. Biochem. 124:201-208(1982)";  };   
             {Name = "Retention coefficient in HPLC, pH 2.1."; Ala = -0.10; Arg = -4.50; Asn = -1.60; Asp = -2.80; Cys = -2.50; Gln = -2.50; Glu = -7.50; Gly = -0.50; His = 0.80; 
                Ile = 11.80; Leu = 10.00; Lys = -3.20; Met = 7.10; Phe = 13.90; Pro = 8.00; Ser = -3.70; Thr = 1.50; Trp = 18.10; Tyr = 8.20; Val = 3.30; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Meek J.L."; Reference = "Proc. Natl. Acad. Sci. USA 77:1632-1636(1980)";  };    
             {Name = "Molar fraction (%) of 2001 buried residues"; Ala = 11.20; Arg = 0.50; Asn = 2.90; Asp = 2.90; Cys = 4.10; Gln = 1.60; Glu = 1.80; Gly = 11.80; His = 2.00; 
                Ile = 8.60; Leu = 11.70; Lys = 0.50; Met = 1.90; Phe = 5.10; Pro = 2.70; Ser = 8.00; Thr = 4.90; Trp = 2.20; Tyr = 2.60; Val = 12.90; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Janin J."; Reference = "Nature 277:491-492(1979)";  };    
             {Name = "Proportion of residues 95% buried (in 12 proteins)"; Ala = 0.38; Arg = 0.01; Asn = 0.12; Asp = 0.15; Cys = 0.50; Gln = 0.07; Glu = 0.18; Gly = 0.36; His = 0.17; 
                Ile = 0.60; Leu = 0.45; Lys = 0.03; Met = 0.40; Phe = 0.50; Pro = 0.18; Ser = 0.22; Thr = 0.23; Trp = 0.27; Tyr = 0.15; Val = 0.54; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = " Chothia C."; Reference = "J. Mol. Biol. 105:1-14(1976)";  };    
             {Name = "Atomic weight ratio of hetero elements in end group to C in side chain"; Ala = 0.00; Arg = 0.65; Asn = 1.33; Asp = 1.38; Cys = 2.75; Gln = 0.89; Glu = 0.92; Gly = 0.74; His = 0.58; 
                Ile = 0.00; Leu = 0.00; Lys = 0.33; Met = 0.00; Phe = 0.00; Pro = 0.39; Ser = 1.42; Thr = 0.71; Trp = 0.13; Tyr = 0.20; Val = 0.00; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = " Grantham R."; Reference = " Science 185:862-864(1974)";  };         
             {Name = "Average flexibility index"; Ala = 0.36; Arg = 0.53; Asn = 0.46; Asp = 0.51; Cys = 0.35; Gln = 0.49; Glu = 0.50; Gly = 0.54; His = 0.32; 
                Ile = 0.46; Leu = 0.37; Lys = 0.47; Met = 0.30; Phe = 0.31; Pro = 0.51; Ser = 0.51; Thr = 0.44; Trp = 0.31; Tyr = 0.42; Val = 0.39; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Bhaskaran R., Ponnuswamy P.K."; Reference = " Int. J. Pept. Protein. Res. 32:242-255(1988)";  };    
             {Name = "Conformational parameter for beta-sheet (computed from 29 proteins)"; Ala = 0.83; Arg = 0.93; Asn = 0.89; Asp = 0.54; Cys = 1.19; Gln = 1.10; Glu = 0.37; Gly = 0.75; His = 0.87; 
                Ile = 1.60; Leu = 1.30; Lys = 0.74; Met = 1.05; Phe = 1.38; Pro = 0.55; Ser = 0.75; Thr = 1.19; Trp = 1.37; Tyr = 1.47; Val = 1.70; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Chou P.Y., Fasman G.D."; Reference = "  Adv. Enzym. 47:45-148(1978)";  };    
             {Name = "Conformational parameter for alpha helix"; Ala = 1.489; Arg = 1.224; Asn = 0.772; Asp = 0.924; Cys = 0.966; Gln = 1.164; Glu = 1.504; Gly = 0.510; His = 1.003; 
                Ile = 1.003; Leu = 1.236; Lys = 1.172; Met = 1.363; Phe = 1.195; Pro = 0.492; Ser = 0.739; Thr = 0.785; Trp = 1.090; Tyr = 0.787; Val = 0.990; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Deleage G., Roux B. "; Reference = " Protein Engineering 1:289-294(1987)";  };    
             {Name = "Conformational parameter for beta-turn"; Ala = 0.788; Arg = 0.912; Asn = 1.572; Asp = 1.197; Cys = 0.965; Gln = 0.997; Glu = 1.149; Gly = 1.860; His = 0.970; 
                Ile = 0.240; Leu = 0.670; Lys = 1.302; Met = 0.436; Phe = 0.624; Pro = 1.415; Ser = 1.316; Thr = 0.739; Trp = 0.546; Tyr = 0.795; Val = 0.387; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Deleage G., Roux B."; Reference = "Protein Engineering 1:289-294(1987)";  };  
            {Name = "Normalized frequency for alpha helix"; Ala = 1.29; Arg = 0.96; Asn = 0.90; Asp = 1.04; Cys = 1.10; Gln = 1.27; Glu = 1.44; Gly = 0.56; His = 1.22; 
                Ile = 0.97; Leu = 1.30; Lys = 1.23; Met = 1.47; Phe = 1.07; Pro = 0.52; Ser = 0.82; Thr = 0.82; Trp = 0.99; Tyr = 0.72; Val = 0.91; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Levitt M."; Reference = "Biochemistry 17:4277-4285(1978)";  };  
            {Name = "Normalized frequency for beta-turn"; Ala = 0.77; Arg = 0.88; Asn = 1.28; Asp = 1.41; Cys = 0.81; Gln = 0.98; Glu = 0.99; Gly = 1.64; His = 0.68; 
                Ile = 0.51; Leu = 0.58; Lys = 0.96; Met = 0.41; Phe = 0.59; Pro = 1.91; Ser = 1.32; Thr = 1.04; Trp = 0.76; Tyr = 1.05; Val = 0.47; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Levitt M."; Reference = "Biochemistry 17:4277-4285(1978)";  };  
            {Name = "Conformational preference for antiparallel beta strand"; Ala = 0.90; Arg = 1.02; Asn = 0.62; Asp = 0.47; Cys = 1.24; Gln = 1.18; Glu = 0.62; Gly = 0.56; His = 1.12; 
                Ile = 1.54; Leu = 1.26; Lys = 0.74; Met = 1.09; Phe = 1.23; Pro = 0.42; Ser = 0.87; Thr = 1.30; Trp = 1.75; Tyr = 1.68; Val = 1.53; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Lifson S., Sander C."; Reference = "Nature 282:109-111(1979)";  };  
            {Name = "Overall amino acid composition (%)"; Ala = 8.30; Arg = 5.70; Asn = 4.40; Asp = 5.30; Cys = 1.70; Gln = 4.00; Glu = 6.20; Gly = 7.20; His = 2.20; 
                Ile = 5.20; Leu = 9.00; Lys = 5.70; Met = 2.40; Phe = 3.90; Pro = 5.10; Ser = 6.90; Thr = 5.80; Trp = 1.30; Tyr = 3.20; Val = 6.60; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "McCaldon P., Argos P."; Reference = "Proteins: Structure, Function and Genetics 4:99-122(1988)";  };
            {Name = "Relative mutability of amino acids (Ala=100)"; Ala = 100.00; Arg = 65.00; Asn = 134.00; Asp = 106.00; Cys = 20.00; Gln = 93.00; Glu = 102.00; Gly = 49.00; His = 66.00; 
                Ile = 96.00; Leu = 40.00; Lys = 56.00; Met = 94.00; Phe = 41.00; Pro = 56.00; Ser = 120.00; Thr = 97.00; Trp = 18.00; Tyr = 41.00; Val = 74.00; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Dayhoff M.O., Schwartz R.M., Orcutt B.C."; Reference = "Atlas of Protein Sequence and Structure, Vol.5, Suppl.3 (1978)";  };
            {Name = "Number of codon(s) coding for each amino acid in universal genetic code"; Ala = 4.000; Arg = 6.000; Asn = 2.000; Asp = 2.000; Cys = 1.000; Gln = 2.000; Glu = 2.000; Gly = 4.000; His = 2.000;
                Ile = 3.000; Leu = 6.000; Lys = 2.000; Met = 1.000; Phe = 2.000; Pro = 4.000; Ser = 6.000; Thr = 4.000; Trp = 1.000; Tyr = 2.000;
                Val = 4.000; Sel = nan; Pyl = nan; Xaa = nan; Glx = nan; Asx = nan; Xle = nan; Author = "-"; Reference = "Most textbooks";};
            {Name = "Polarity"; Ala = 0.000; Arg = 52.000; Asn = 3.380; Asp = 49.700; Cys = 1.480; Gln = 3.530; Glu = 49.900; Gly = 0.000; His = 51.600; 
                Ile = 0.130; Leu = 0.130; Lys = 49.500; Met = 1.430; Phe = 0.350; Pro = 1.580; Ser = 1.670; Thr = 1.660; Trp = 2.100; Tyr = 1.610; Val = 0.130; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Zimmerman J.M., Eliezer N., Simha R."; Reference = "J. Theor. Biol. 21:170-201(1968)";};
            {Name = "Refractivity"; Ala = 4.340; Arg = 26.660; Asn = 13.280; Asp = 12.000; Cys = 35.770; Gln = 17.560; Glu = 17.260; Gly = 0.000; His = 21.810;
                Ile = 19.060; Leu = 18.780; Lys = 21.290; Met = 21.640; Phe = 29.400; Pro = 10.930; Ser = 6.350; Thr = 11.010; Trp = 42.530; Tyr = 31.530; Val = 13.920; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Jones. D.D."; Reference = "J. Theor. Biol. 50:167-184(1975)";};
            {Name = "Normalized consensus hydrophobicity scale"; Ala = 0.620; Arg = -2.530; Asn = -0.780; Asp = -0.900; Cys = 0.290; Gln = -0.850; Glu = -0.740; Gly = 0.480; His = -0.400;
                Ile = 1.380; Leu = 1.060; Lys = -1.500; Met = 0.640; Phe = 1.190; Pro = 0.120; Ser = -0.180; Thr = -0.050; Trp = 0.810; Tyr = 0.260; Val = 1.080; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Eisenberg D., Schwarz E., Komarony M., Wall R."; Reference = "J. Mol. Biol. 179:125-142(1984)";};
            {Name = "Hydrophilicity"; Ala = -0.500; Arg = 3.000; Asn = 0.200; Asp = 3.000; Cys = -1.000; Gln = 0.200; Glu = 3.000; Gly = 0.000; His = -0.500;
                Ile = -1.800; Leu = -1.800; Lys = 3.000; Met = -1.300; Phe = -2.500; Pro = 0.000; Ser = 0.300; Thr = -0.400; Trp = -3.400; Tyr = -2.300; Val = -1.500; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Hopp T.P., Woods K.R."; Reference = "Proc. Natl. Acad. Sci. U.S.A. 78:3824-3828(1981)";};
            {Name = "Average surrounding hydrophobicity"; Ala = 12.970; Arg = 11.720; Asn = 11.420; Asp = 10.850; Cys = 14.630; Gln = 11.760; Glu = 11.890; Gly = 12.430; His = 12.160;
                Ile = 15.670; Leu = 14.900; Lys = 11.360; Met = 14.390; Phe = 14.000; Pro = 11.370; Ser = 11.230; Thr = 11.690; Trp = 13.930; Tyr = 13.420; Val = 15.710; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Manavalan P., Ponnuswamy P.K."; Reference = "Nature 275:673-674(1978)";};
            {Name = "Hydrophobicity of physiological L-alpha amino acids"; Ala = 0.616; Arg = 0.000; Asn = 0.236; Asp = 0.028; Cys = 0.680; Gln = 0.251; Glu = 0.043; Gly = 0.501;
                His = 0.165; Ile = 0.943; Leu = 0.943; Lys = 0.283; Met = 0.738; Phe = 1.000; Pro = 0.711; Ser = 0.359; Thr = 0.450; Trp = 0.878; Tyr = 0.880; Val = 0.825; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Black S.D., Mould D.R."; Reference = "Anal. Biochem. 193:72-82(1991)";};
            {Name = "Hydrophobicity scale (pi-r)"; Ala = 0.310; Arg = -1.010; Asn = -0.600; Asp = -0.770; Cys = 1.540; Gln = -0.220; Glu = -0.640; Gly = 0.000; His = 0.130;
                Ile = 1.800; Leu = 1.700; Lys = -0.990; Met = 1.230; Phe = 1.790; Pro = 0.720; Ser = -0.040; Thr = 0.260; Trp = 2.250; Tyr = 0.960; Val = 1.220; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Fauchere J.-L., Pliska V.E."; Reference = "Eur. J. Med. Chem. 18:369-375(1983)";};
            {Name = "Free energy of transfer from inside to outside of a globular protein"; Ala = 0.300; Arg = -1.400; Asn = -0.500; Asp = -0.600; Cys = 0.900; Gln = -0.700;
                Glu = -0.700; Gly = 0.300; His = -0.100; Ile = 0.700; Leu = 0.500; Lys = -1.800; Met = 0.400; Phe = 0.500; Pro = -0.300; Ser = -0.100; Thr = -0.200; Trp = 0.300; Tyr = -0.400;
                Val = 0.600; Sel = nan; Pyl = nan; Xaa = nan; Glx = nan; Asx = nan; Xle = nan; Author = "Janin J."; Reference = "Nature 277:491-492(1979)";};
            {Name = "Membrane buried helix parameter"; Ala = 1.360; Arg = 0.150; Asn = 0.330; Asp = 0.110; Cys = 1.270; Gln = 0.330; Glu = 0.250; Gly = 1.090; His = 0.680;
                Ile = 1.440; Leu = 1.470; Lys = 0.090; Met = 1.420; Phe = 1.570; Pro = 0.540; Ser = 0.970; Thr = 1.080; Trp = 1.000; Tyr = 0.830; Val = 1.370; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Rao M.J.K., Argos P."; Reference = "Biochim. Biophys. Acta 869:197-214(1986)";};
            {Name = "Hydrophobicity scale (Contribution of hydrophobic interactions to the stability of the globular conformation of proteins)"; Ala = 0.620; Arg = -2.530; Asn = -0.780; Asp = -0.090;
                Cys = 0.290; Gln = -0.850; Glu = -0.740; Gly = 0.480; His = -0.400; Ile = 1.380; Leu = 1.530; Lys = -1.500; Met = 0.640; Phe = 1.190; Pro = 0.120; Ser = -0.180;
                Thr = -0.050; Trp = 0.810; Tyr = 0.260; Val = 1.800; Sel = nan; Pyl = nan; Xaa = nan; Glx = nan; Asx = nan; Xle = nan; Author = "Tanford C."; Reference = "J. Am. Chem. Soc. 84:4240-4274(1962)";};
            {Name = "Antigenicity value X 10"; Ala = 1.150; Arg = 0.580; Asn = -0.770; Asp = 0.650; Cys = -1.200; Gln = -0.110; Glu = -0.710; Gly = -1.840; His = 3.120;
                Ile = -2.920; Leu = 0.750; Lys = 2.060; Met = -3.850; Phe = -1.410; Pro = -0.530; Ser = -0.260; Thr = -0.450;Trp = -1.140; Tyr = 0.130; Val = -0.130; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Welling G.W., Weijer W.J., Van der Zee R., Welling-Wester S."; Reference = "FEBS Lett. 188:215-218(1985)";};
            {Name = "Hydrophilicity scale derived from HPLC peptide retention times"; Ala = 2.100; Arg = 4.200; Asn = 7.000; Asp = 10.000; Cys = 1.400; Gln = 6.000; Glu = 7.800; Gly = 5.700;
                His = 2.100; Ile = -8.000; Leu = -9.200; Lys = 5.700; Met = -4.200; Phe = -9.200; Pro = 2.100; Ser = 6.500; Thr = 5.200; Trp = -10.000; Tyr = -1.900; Val = -3.700; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Parker J.M.R., Guo D., Hodges R.S."; Reference = "Biochemistry 25:5425-5431(1986)";};
            {Name = "Hydrophobicity indices at ph 7.5 determined by HPLC"; Ala = 0.350; Arg = -1.500; Asn = -0.990; Asp = -2.150; Cys = 0.760; Gln = -0.930; Glu = -1.950; Gly = 0.000;
                His = -0.650; Ile = 1.830; Leu = 1.800; Lys = -1.540; Met = 1.100; Phe = 1.690; Pro = 0.840; Ser = -0.630; Thr = -0.270; Trp = 1.350; Tyr = 0.390; Val = 1.320; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Cowan R., Whittaker R.G."; Reference = "Peptide Research 3:75-80(1990)";};
            {Name = "Retention coefficient in HFBA"; Ala = 3.900; Arg = 3.200; Asn = -2.800; Asp = -2.800; Cys = -14.300; Gln = 1.800; Glu = -7.500; Gly = -2.300; His = 2.000;
                Ile = 11.000; Leu = 15.000; Lys = -2.500; Met = 4.100; Phe = 14.700; Pro = 5.600; Ser = -3.500; Thr = 1.100; Trp = 17.800; Tyr = 3.800; Val = 2.100; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Browne C.A., Bennett H.P.J., Solomon S."; Reference = "Anal. Biochem. 124:201-208(1982)";};
            {Name = "Transmembrane tendency"; Ala = 0.380; Arg = -2.570; Asn = -1.620; Asp = -3.270; Cys = -0.300; Gln = -1.840; Glu = -2.900; Gly = -0.190; His = -1.440;
                Ile = 1.970; Leu = 1.820; Lys = -3.460; Met = 1.400; Phe = 1.980; Pro = -1.440; Ser = -0.530; Thr = -0.320; Trp = 1.530; Tyr = 0.490; Val = 1.460; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Zhao, G., London E."; Reference = "Protein Sci. 15:1987-2001(2006)";};
            {Name = "Retention coefficient in HPLC, pH 7.4"; Ala = 0.500; Arg = 0.800; Asn = 0.800; Asp = -8.200; Cys = -6.800; Gln = -4.800; Glu = -16.900; Gly = 0.000;
                His = -3.500; Ile = 13.900; Leu = 8.800; Lys = 0.100; Met = 4.800; Phe = 13.200; Pro = 6.100; Ser = 1.200; Thr = 2.700; Trp = 14.900; Tyr = 6.100; Val = 2.700; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Meek J.L."; Reference = "Proc. Natl. Acad. Sci. USA 77:1632-1636(1980)";};
            {Name = "Molar fraction (%) of 3220 accessible residues"; Ala = 6.600; Arg = 4.500; Asn = 6.700; Asp = 7.700; Cys = 0.900; Gln = 5.200; Glu = 5.700; Gly = 6.700; His = 2.500;
                Ile = 2.800; Leu = 4.800; Lys = 10.300; Met = 1.000; Phe = 2.400; Pro = 4.800; Ser = 9.400; Thr = 7.000; Trp = 1.400; Tyr = 5.100; Val = 4.500; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Janin J."; Reference = "Nature 277:491-492(1979)";};
            {Name = "Mean fractional area loss (f) [average area buried/standard state area]"; Ala = 0.740; Arg = 0.640; Asn = 0.630; Asp = 0.620; Cys = 0.910; Gln = 0.620; Glu = 0.620; Gly = 0.720; His = 0.780;
                Ile = 0.880; Leu = 0.850; Lys = 0.520; Met = 0.850; Phe = 0.880; Pro = 0.640; Ser = 0.660; Thr = 0.700; Trp = 0.850; Tyr = 0.760; Val = 0.860; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Rose G.D., Geselowitz A.R., Lesser G.J., Lee R.H., Zehfus M.H."; Reference = "Science 229:834-838(1985)";};
            {Name = "Average area buried on transfer from standard state to folded protein"; Ala = 86.600; Arg = 162.200; Asn = 103.300; Asp = 97.800; Cys = 132.300; Gln = 119.200; Glu = 113.900; Gly = 62.900; His = 155.800;
                Ile = 158.000; Leu = 164.100; Lys = 115.500; Met = 172.900; Phe = 194.100; Pro = 92.900; Ser = 85.600; Thr = 106.500; Trp = 224.600; Tyr = 177.700; Val = 141.000; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Rose G.D., Geselowitz A.R., Lesser G.J., Lee R.H., Zehfus M.H."; Reference = "Science 229:834-838(1985)";};
            {Name = "Conformational parameter for alpha helix (computed from 29 proteins)"; Ala = 1.420; Arg = 0.980; Asn = 0.670; Asp = 1.010; Cys = 0.700; Gln = 1.110; Glu = 1.510; Gly = 0.570; His = 1.000;
                Ile = 1.080; Leu = 1.210; Lys = 1.160; Met = 1.450; Phe = 1.130; Pro = 0.570; Ser = 0.770; Thr = 0.830; Trp = 1.080; Tyr = 0.690; Val = 1.060; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Chou P.Y., Fasman G.D."; Reference = "Adv. Enzym. 47:45-148(1978)";};
            {Name = "Conformational parameter for beta-turn (computed from 29 proteins)"; Ala = 0.660; Arg = 0.950; Asn = 1.560; Asp = 1.460; Cys = 1.190; Gln = 0.980; Glu = 0.740; Gly = 1.560; His = 0.950;
                Ile = 0.470; Leu = 0.590; Lys = 1.010; Met = 0.600; Phe = 0.600; Pro = 1.520; Ser = 1.430; Thr = 0.960; Trp = 0.960; Tyr = 1.140; Val = 0.500; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Chou P.Y., Fasman G.D."; Reference = "Adv. Enzym. 47:45-148(1978)";};
            {Name = "Conformational parameter for beta-sheet"; Ala = 0.709; Arg = 0.920; Asn = 0.604; Asp = 0.541; Cys = 1.191; Gln = 0.840; Glu = 0.567; Gly = 0.657; His = 0.863;
                Ile = 1.799; Leu = 1.261; Lys = 0.721; Met = 1.210; Phe = 1.393; Pro = 0.354; Ser = 0.928; Thr = 1.221; Trp = 1.306; Tyr = 1.266; Val = 1.965; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Deleage G., Roux B."; Reference = "Protein Engineering 1:289-294(1987)";};
            {Name = "Conformational parameter for coil"; Ala = 0.824; Arg = 0.893; Asn = 1.167; Asp = 1.197; Cys = 0.953; Gln = 0.947; Glu = 0.761; Gly = 1.251; His = 1.068;
                Ile = 0.886; Leu = 0.810; Lys = 0.897; Met = 0.810; Phe = 0.797; Pro = 1.540; Ser = 1.130; Thr = 1.148; Trp = 0.941; Tyr = 1.109; Val = 0.772; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Deleage G., Roux B."; Reference = "Protein Engineering 1:289-294(1987)";};
            {Name = "Normalized frequency for beta-sheet"; Ala = 0.900; Arg = 0.990; Asn = 0.760; Asp = 0.720; Cys = 0.740; Gln = 0.800; Glu = 0.750; Gly = 0.920; His = 1.080;
                Ile = 1.450; Leu = 1.020; Lys = 0.770; Met = 0.970; Phe = 1.320; Pro = 0.640; Ser = 0.950; Thr = 1.210; Trp = 1.140; Tyr = 1.250; Val = 1.490; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Levitt M."; Reference = "Biochemistry 17:4277-4285(1978)";};
            {Name = "Conformational preference for total beta strand (antiparallel+parallel)"; Ala = 0.920; Arg = 0.930; Asn = 0.600; Asp = 0.480; Cys = 1.160; Gln = 0.950; Glu = 0.610; Gly = 0.610; His = 0.930;
                Ile = 1.810; Leu = 1.300; Lys = 0.700; Met = 1.190; Phe = 1.250; Pro = 0.400; Ser = 0.820; Thr = 1.120; Trp = 1.540; Tyr = 1.530; Val = 1.810; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Lifson S., Sander C."; Reference = "Nature 282:109-111(1979)";};
            {Name = "Conformational preference for parallel beta strand"; Ala = 1.000; Arg = 0.680; Asn = 0.540; Asp = 0.500; Cys = 0.910; Gln = 0.280; Glu = 0.590; Gly = 0.790; His = 0.380;
                Ile = 2.600; Leu = 1.420; Lys = 0.590; Met = 1.490; Phe = 1.300; Pro = 0.350; Ser = 0.700; Thr = 0.590; Trp = 0.890; Tyr = 1.080; Val = 2.630; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Lifson S., Sander C."; Reference = "Nature 282:109-111(1979)";};
            {Name = "Amino acid composition (%) in the UniProtKB/Swiss-Prot data bank."; Ala = 8.25; Arg = 5.53; Asn = 4.06; Asp = 5.45; Cys = 1.37; Gln = 3.93; Glu = 6.75; Gly = 7.07; His = 2.27;
                Ile = 5.96; Leu = 9.66; Lys = 5.84; Met = 2.42; Phe = 3.86; Pro = 4.70; Ser = 6.56; Thr = 5.34; Trp = 1.08; Tyr = 2.92; Val = 6.87; Sel = nan; Pyl = nan; Xaa = nan; 
                Glx = nan; Asx = nan; Xle = nan; Author = "Bairoch A."; Reference = "Release notes for UniProtKB/Swiss-Prot release 2013_04 - April 2013";};
            ]|> List.map (fun pd -> pd.Name,pd) |> Map.ofList           
     

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
              
     
        let private getProperties (ps:PropertyDescription) (aa:AminoAcidLiteral) =
                match aa with
                | AminoAcidLiteral.Ala -> ps.Ala
                | AminoAcidLiteral.Cys -> ps.Cys
                | AminoAcidLiteral.Asp -> ps.Asp
                | AminoAcidLiteral.Glu -> ps.Glu
                | AminoAcidLiteral.Phe -> ps.Phe
                | AminoAcidLiteral.Gly -> ps.Gly
                | AminoAcidLiteral.His -> ps.His
                | AminoAcidLiteral.Ile -> ps.Ile
                | AminoAcidLiteral.Lys -> ps.Lys
                | AminoAcidLiteral.Leu -> ps.Leu
                | AminoAcidLiteral.Met -> ps.Met
                | AminoAcidLiteral.Asn -> ps.Asn
                | AminoAcidLiteral.Pyl -> ps.Pyl
                | AminoAcidLiteral.Pro -> ps.Pro
                | AminoAcidLiteral.Gln -> ps.Gln
                | AminoAcidLiteral.Arg -> ps.Arg
                | AminoAcidLiteral.Ser -> ps.Ser
                | AminoAcidLiteral.Thr -> ps.Thr
                | AminoAcidLiteral.Sel -> ps.Sel
                | AminoAcidLiteral.Val -> ps.Val
                | AminoAcidLiteral.Trp -> ps.Trp
                | AminoAcidLiteral.Tyr -> ps.Tyr
               
                | AminoAcidLiteral.Xaa -> ps.Xaa
                | AminoAcidLiteral.Xle -> ps.Xle
                | AminoAcidLiteral.Glx -> ps.Glx
                | AminoAcidLiteral.Asx -> ps.Asx
               
                | AminoAcidLiteral.Gap -> nan
                | AminoAcidLiteral.Ter -> nan
               

        //Function for looking up an item based on the name. 
        let hydrophobicity (aa:AminoAcidLiteral) = getProperties (propertyDescriptionMap.Item("Hydrophobicity")) aa
        let hydropathicity (aa:AminoAcidLiteral) = getProperties (propertyDescriptionMap.Item("Hydropathicity")) aa
        let omh (aa:AminoAcidLiteral) = getProperties (propertyDescriptionMap.Item("Optimized Matching Hydrophobicity")) aa

        let bulkiness aa = getProperties (propertyDescriptionMap.Item("Bulkiness")) aa
        let granthamPolarity aa = getProperties (propertyDescriptionMap.Item("Polarity (p)")) aa
        let recFactors aa = getProperties (propertyDescriptionMap.Item("Recognition factors")) aa
        let molecularWeight aa = getProperties (propertyDescriptionMap.Item("Molecular weight")) aa
        let bullHydrophobicity aa= getProperties (propertyDescriptionMap.Item("Hydrophobicity (free energy of transfer to surface in kcal/mole)")) aa
        let guyHydrophobicity aa = getProperties (propertyDescriptionMap.Item("Hydrophobicity scale based on free energy of transfer (kcal/mole)")) aa
        let miyazawaHydrophobicity aa = getProperties (propertyDescriptionMap.Item("Hydrophobicity scale (contact energy derived from 3D data)")) aa
        let rosemanHydrophobicity aa = getProperties (propertyDescriptionMap.Item("Hydrophobicity scale (pi-r)")) aa
        let hydrationPotential aa = getProperties (propertyDescriptionMap.Item("Hydration potential (kcal/mole) at 25øC")) aa
        let constantsHydrophob aa = getProperties (propertyDescriptionMap.Item("Hydrophobic constants derived from HPLC peptide retention times")) aa
        let indicesHydrophob aa = getProperties (propertyDescriptionMap.Item("Hydrophobicity indices at ph 3.4 determined by HPLC")) aa
        let mobility aa = getProperties (propertyDescriptionMap.Item("Mobilities of amino acids on chromatography paper (RF)")) aa
        let tfaRtCoeff aa = getProperties (propertyDescriptionMap.Item("Retention coefficient in TFA")) aa
        let hplcPh2rtCoeff aa = getProperties (propertyDescriptionMap.Item("Retention coefficient in HPLC, pH 2.1.")) aa
        let molFraction2001BuriedRes aa = getProperties (propertyDescriptionMap.Item("Molar fraction (%) of 2001 buried residues")) aa
        let proportionOfRes95Buried aa = getProperties (propertyDescriptionMap.Item("Proportion of residues 95% buried (in 12 proteins)")) aa
        let atomicWeightRatioHeteroElem aa = getProperties (propertyDescriptionMap.Item("Atomic weight ratio of hetero elements in end group to C in side chain")) aa
        let avgFlexibilityIndex aa = getProperties (propertyDescriptionMap.Item("Average flexibility index")) aa
        let betaSheet29 aa = getProperties (propertyDescriptionMap.Item("Conformational parameter for beta-sheet (computed from 29 proteins)")) aa
        let alphaHelix aa = getProperties (propertyDescriptionMap.Item("Conformational parameter for alpha helix")) aa
        let betaTurn aa = getProperties (propertyDescriptionMap.Item("Conformational parameter for beta-turn")) aa
        let normFrequencyAlphaHelix aa = getProperties (propertyDescriptionMap.Item("Normalized frequency for alpha helix")) aa
        let normFrequencyBetaTurn aa = getProperties (propertyDescriptionMap.Item("Normalized frequency for beta-turn")) aa
        let antiparallelBetaConfPreference aa = getProperties (propertyDescriptionMap.Item("Conformational preference for antiparallel beta strand")) aa 
        let overallAaComp aa = getProperties (propertyDescriptionMap.Item("Overall amino acid composition (%)")) aa
        let relMutability aa = getProperties (propertyDescriptionMap.Item("Relative mutability of amino acids (Ala=100)")) aa
        let numberOfCodonsUniversalGenCode aa = getProperties (propertyDescriptionMap.Item ("Number of codon(s) coding for each amino acid in universal genetic code")) aa
        let zimmermanPolarity aa = getProperties (propertyDescriptionMap.Item("Polarity")) aa
        let refractivity aa = getProperties (propertyDescriptionMap.Item("Refractivity")) aa
        let normConsensusHydrophob aa = getProperties (propertyDescriptionMap.Item("Normalized consensus hydrophobicity scale")) aa
        let hydrophilicity aa = getProperties (propertyDescriptionMap.Item("Hydrophilicity")) aa
        let avgSorroundingHydrophob aa = getProperties (propertyDescriptionMap.Item("Average surrounding hydrophobicity")) aa
        let lAlphaHydrophobicity aa = getProperties (propertyDescriptionMap.Item("Hydrophobicity of physiological L-alpha amino acids")) aa
        let fauchereHydrophobicity aa = getProperties (propertyDescriptionMap.Item("Hydrophobicity scale (pi-r)")) aa 
        let freeEnergyofTransfer aa = getProperties (propertyDescriptionMap.Item("Free energy of transfer from inside to outside of a globular protein")) aa
        let membraneBuriedHelixParam aa = getProperties (propertyDescriptionMap.Item("Membrane buried helix parameter")) aa
        let contribHydrophobInteractionsToStability aa = getProperties (propertyDescriptionMap.Item("Hydrophobicity scale (Contribution of hydrophobic interactions to the stability of the globular conformation of proteins)")) aa
        let antigenicity aa = getProperties (propertyDescriptionMap.Item( "Antigenicity value X 10")) aa
        let hydrophilicityScaleHPLCRtTime aa = getProperties (propertyDescriptionMap.Item("Hydrophilicity scale derived from HPLC peptide retention times")) aa
        let hydrophobIndex aa = getProperties (propertyDescriptionMap.Item("Hydrophobicity indices at ph 7.5 determined by HPLC")) aa
        let hfbaRtCoeff aa = getProperties (propertyDescriptionMap.Item("Retention coefficient in HFBA")) aa
        let transmembraneTendency aa = getProperties (propertyDescriptionMap.Item("Transmembrane tendency")) aa
        let hplcPH7RtCoeff aa = getProperties (propertyDescriptionMap.Item("Retention coefficient in HPLC, pH 7.4")) aa 
        let molFractionAccessibleResidues aa = getProperties (propertyDescriptionMap.Item("Molar fraction (%) of 3220 accessible residues")) aa
        let meanFractionalAreaLoss aa = getProperties (propertyDescriptionMap.Item("Mean fractional area loss (f) [average area buried/standard state area]")) aa
        let avgBuriedArea aa = getProperties (propertyDescriptionMap.Item("Average area buried on transfer from standard state to folded protein")) aa
        let alphaHelix29 aa = getProperties (propertyDescriptionMap.Item("Conformational parameter for alpha helix (computed from 29 proteins)")) aa
        let betaTurn29 aa = getProperties (propertyDescriptionMap.Item ("Conformational parameter for beta-turn (computed from 29 proteins)")) aa
        let betaSheet aa = getProperties (propertyDescriptionMap.Item("Conformational parameter for beta-sheet")) aa
        let coil aa = getProperties (propertyDescriptionMap.Item("Conformational parameter for coil")) aa
        let normFrequencyBetaSheet aa = getProperties (propertyDescriptionMap.Item("Normalized frequency for beta-sheet")) aa
        let totalBetaStrandConfPreference aa = getProperties (propertyDescriptionMap.Item("Conformational preference for total beta strand (antiparallel+parallel)")) aa
        let parallelBetaStrandConfPreference aa = getProperties (propertyDescriptionMap.Item("Conformational preference for parallel beta strand")) aa
        let aaCompUniSwissProt aa = getProperties (propertyDescriptionMap.Item("Amino acid composition (%) in the UniProtKB/Swiss-Prot data bank.")) aa