namespace FSharpBio.Mz

open FSharpBio
open AminoAcids
open Spectra

module Fragmentation =
    
   
    /// <summary>
    ///   Computes uncharged mass series of a-,b-,c- ions
    /// </summary>
    ///
    /// <param name="aal">list of aminoacids/param>
    /// <remarks>result is uncharged use <c>Mass.toMZ</c></remarks>
    /// <returns>uncharged mass series of a-,b-,c- ions</returns>   
    let abcSeries  (aal:AminoAcids.AminoAcid list) =    
        let rec series aminoList fragMasses acc =
            match aminoList with
            | f::s::rest -> let currentMass = Formula.monoisoMass (AminoAcids.formula f)
                            let a' = acc + currentMass - (Formula.monoisoMass (AminoAcids.isotopicLabelFunc f  Formula.Table.CO))
                            let b' = acc + currentMass 
                            let c' = acc + currentMass + (Formula.monoisoMass (AminoAcids.isotopicLabelFunc s Formula.Table.NH3))
                            series (s::rest) ((a',b',c')::fragMasses) b'
            | f::rest    -> let currentMass = Formula.monoisoMass (AminoAcids.formula f)
                            let a' = acc + currentMass - (Formula.monoisoMass (AminoAcids.isotopicLabelFunc f Formula.Table.CO))
                            let b' = acc + currentMass 
                            let c' = nan
                            series rest ((a',b',c')::fragMasses) b'
            | _          ->  fragMasses
        (series aal [] 0.0) |> List.rev


    /// <summary>
    ///   Computes uncharged mass series of x-,y-,z- ions
    /// </summary>
    ///
    /// <param name="aal"> List of aminoacids </param>
    /// <remarks> Result is uncharged use <c>Mass.toMZ</c> </remarks>
    /// <returns> Uncharged mass series of x-,y-,z- ions </returns>   
    let xyzSeries  (aal:AminoAcids.AminoAcid list) =    
        let rec series aminoList fragMasses acc =
            match aminoList with
            | f::s::rest -> let currentMass = Formula.monoisoMass (AminoAcids.formula f)
                            let x' = acc + currentMass + (Formula.monoisoMass (AminoAcids.isotopicLabelFunc f Formula.Table.CO)) 
                            let y' = acc + currentMass 
                            let z' = acc + currentMass - (Formula.monoisoMass (AminoAcids.isotopicLabelFunc f Formula.Table.NH3))
                            series (s::rest) ((x',y',z')::fragMasses) y'
            | f::rest    -> let currentMass = Formula.monoisoMass (AminoAcids.formula f)
                            let x' = nan
                            let y' = acc + currentMass
                            let z' = acc + currentMass - (Formula.monoisoMass (AminoAcids.isotopicLabelFunc f Formula.Table.NH3))
                            series rest ((x',y',z')::fragMasses) y'
            | _          -> fragMasses
        (series (aal|> List.rev) [] (Formula.monoisoMass Formula.Table.H2O))  


    /// <summary>
    ///   Computes uncharged mass series of x-,y-,z- ions
    ///   with neutral water loss
    /// </summary>
    ///
    /// <param name="aal"> List of aminoacids </param>
    /// <remarks> Result is uncharged use <c>Mass.toMZ</c> </remarks>
    /// <returns> Uncharged mass series of x-,y-,z- ions with neutral water loss </returns>   
    //  a,b,c with neutral water loss do not exist (except the residual dependant onces)
    let xyzSeriesWaterLoss (aal:AminoAcids.AminoAcid list) =    
        let rec series aminoList fragMasses acc =
            match aminoList with
            | f::s::rest -> let currentMass = Formula.monoisoMass (AminoAcids.formula f)
                            let x' = acc + currentMass + (Formula.monoisoMass (AminoAcids.isotopicLabelFunc f Formula.Table.CO)) 
                            let y' = acc + currentMass 
                            let z' = acc + currentMass - (Formula.monoisoMass (AminoAcids.isotopicLabelFunc f Formula.Table.NH3))
                            series (s::rest) ((x',y',z')::fragMasses) y'
            | f::rest    -> let currentMass = Formula.monoisoMass (AminoAcids.formula f)
                            let x' = nan
                            let y' = acc + currentMass
                            let z' = acc + currentMass - (Formula.monoisoMass (AminoAcids.isotopicLabelFunc f Formula.Table.NH3))
                            series rest ((x',y',z')::fragMasses) y'
            | _          -> fragMasses
        (series (aal|> List.rev) [] (0.0))  

    

    /// <summary>
    ///   Computes uncharged mass series of a-,b-,c- ions
    ///   with neutral ammonia loss
    /// </summary>
    ///
    /// <param name="aal"> List of aminoacids </param>
    /// <remarks> Result is uncharged use <c>Mass.toMZ</c> </remarks>
    /// <returns> Uncharged mass series of a-,b-,c- ions with neutral ammonia loss </returns>   
    //  x,y,z with neutral amonium loss do not exist (except the residual dependant onces)
    let abcSeriesAmmoniaLoss (aal:AminoAcids.AminoAcid list) = 
        let rec series aminoList fragMasses acc =
            match aminoList with
            | f::rest    -> let currentMass = Formula.monoisoMass (AminoAcids.formula f)                            
                            let a' = acc + currentMass - (Formula.monoisoMass (AminoAcids.isotopicLabelFunc f Formula.Table.CO))
                            let b' = acc + currentMass
                            let c' = nan
                            series rest ((a',b',c')::fragMasses) b'
            | _          ->  fragMasses
        
        if aal.Length > 0 then
            let fistAmino = List.head aal
            let ammonia = (Formula.monoisoMass (AminoAcids.isotopicLabelFunc fistAmino Formula.Table.NH3))
            (series aal [] (- ammonia)) |> List.rev
        else
            []



               
//    let imoniumIons (rawMass:List<float>) (label : Formula.Formula -> Formula.Formula) = 
//        let currentCO = massDiffAX_CO label 
//        rawMass |> List.map (fun n ->  n - currentCO)
        

    
    let isAminoLoss (a:AminoAcids.AminoAcid) =
        match a with
        | AminoAcid.Literal (a)    when a = AminoAcidLiteral.Arg || a =  AminoAcidLiteral.Lys || a = AminoAcidLiteral.Gln || a = AminoAcidLiteral.Asn   -> true
        | AminoAcid.Isotopic (a,i) when a = AminoAcidLiteral.Arg || a =  AminoAcidLiteral.Lys || a = AminoAcidLiteral.Gln || a = AminoAcidLiteral.Asn   -> true
        | AminoAcid.Modified (a,m) when a = AminoAcidLiteral.Arg || a =  AminoAcidLiteral.Lys || a = AminoAcidLiteral.Gln || a = AminoAcidLiteral.Asn   -> true
        | AminoAcid.Both (a,i,m)   when a = AminoAcidLiteral.Arg || a =  AminoAcidLiteral.Lys || a = AminoAcidLiteral.Gln || a = AminoAcidLiteral.Asn   -> true
        | _ -> false

    let isWaterLoss (a:AminoAcids.AminoAcid) =
        match a with
        | AminoAcid.Literal (a)    when a = AminoAcidLiteral.Ser || a = AminoAcidLiteral.Thr || a = AminoAcidLiteral.Glu || a = AminoAcidLiteral.Asp   -> true
        | AminoAcid.Isotopic (a,i) when a = AminoAcidLiteral.Ser || a = AminoAcidLiteral.Thr || a = AminoAcidLiteral.Glu || a = AminoAcidLiteral.Asp   -> true
        | AminoAcid.Modified (a,m) when a = AminoAcidLiteral.Ser || a = AminoAcidLiteral.Thr || a = AminoAcidLiteral.Glu || a = AminoAcidLiteral.Asp   -> true
        | AminoAcid.Both (a,i,m)   when a = AminoAcidLiteral.Ser || a = AminoAcidLiteral.Thr || a = AminoAcidLiteral.Glu || a = AminoAcidLiteral.Asp   -> true
        | _ -> false







