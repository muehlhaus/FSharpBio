namespace FSharpBio.Mz

module IsotopicDistribution = 

    open FSharpBio
    open FSharpBio.Formula

    // Implementation according to BRAIN - algortihm
    let aggregatedIsotopicDistribution (f:Formula) (limit:int) =
        let calcP0 (f:Formula) = 
            f |> Map.fold (fun s k v -> let stdIso = Elements.getMainIsotope k
                                        s * stdIso.NatAbundance**float(v) ) 1.       
        // Calculate coefficient ql (Equ. 12 + 7)
        let calcPhiL (f:Formula) (l:int) = 
            let lf = float l 
            f |> Map.fold (fun s k v -> s + (Elements.getSinglePhiL k (float v) lf ) ) 0.


        let rec calcPJ (ps:List<float>) (phis:List<float>) (state:float)=
            match ps,phis with
            | [],[] -> state
            | pj::ps,phi::phis ->  calcPJ ps phis (state + pj * phi)
            | _ ,[] -> raise ( System.Exception("ps can't be longer then phis see function w") ) //state // TODO: exeption : ps can't be longer then phis see function w
            | [],_  -> state
            

        let rec w (ps:List<float>) (phis:List<float>) (i:int) =
            if ps.Length < phis.Length then
                let np = calcPJ ps phis 0.
                w ((np / - (float i))::ps) phis (i + 1)
            else
                ps

        let allPhis = [ for i in [1..limit] do yield (calcPhiL f i) ]
        let p0 = calcP0 f
        let ps = w [p0] allPhis 1
        ps


    let aggregatedIsotopicMassDistribution (f:Formula) (limit:int) =
        let calcP0 (f:Formula) = 
            f |> Map.fold (fun s k v -> let stdIso = Elements.getMainIsotope k
                                        s * stdIso.NatAbundance**float(v) ) 1.       
        
        let calcPhiL (f:Formula) (l:int) = 
            let lf = float l 
            f |> Map.fold (fun s k v -> s + (Elements.getSinglePhiM k (float v) lf ) ) 0.


        let rec calcPJ (ps:List<float>) (phis:List<float>) (state:float)=
            match ps,phis with
            | [],[] -> state
            | pj::ps,phi::phis ->  calcPJ ps phis (state + pj * phi)
            | _ ,[] -> raise ( System.Exception("ps can't be longer then phis see function w") ) //state // TODO: exeption : ps can't be longer then phis see function w
            | [],_  -> state
            

        let rec w (ps:List<float>) (phis:List<float>) (i:int) =
            if ps.Length < phis.Length then
                let np = calcPJ ps phis 0.
                w ((np / - (float i))::ps) phis (i + 1)
            else
                ps

        let allPhis = [ for i in [1..limit] do yield (calcPhiL f i) ]
        let p0 = calcP0 f
        let ps = w [p0] allPhis 1
        ps

//    // Implementation according to BRAIN - algortihm
//    let aggregatedIsotopicMassDistribution (f:Formula) (limit:int) =
//        let calcP0 (f:Formula) = 
//            f |> Map.fold (fun s k v -> let stdIso = Elements.getMainIsotope k
//                                        float(v) * stdIso.Mass * s * stdIso.NatAbundance**float(v) ) 1.       
//
//        let calcPhiL (f:Formula) (l:int) = 
//            let lf = float l 
//            f |> Map.fold (fun s k v -> s + (Elements.getSinglePhiL k (float v) lf ) ) 0.
//        
//
//        let calcPhiM (f:Formula) (l:int) = 
//            let lf = float l 
//            f |> Map.fold (fun s k v -> //let mcoef = Elements.getCoef  k (float v) lf
//                                        s + ( (Elements.getSinglePhiM k (float v) lf ) ) ) 0.
//
//
//        let rec calcPJ (ps:List<float>) (phis:List<float>) (state:float)=
//            match ps,phis with
//            | [],[] -> state
//            | pj::ps,phi::phis -> //let phiN = fst phi
//                                  //let phiM = snd phi 
//                                  calcPJ ps phis (state + pj * phi ) //(state + (pj * (fst phi)) + (pj * (snd phi)) )
//            | _ ,[] -> raise ( System.Exception("ps can't be longer then phis see function w") ) //state // TODO: exeption : ps can't be longer then phis see function w
//            | [],_  -> state
//            
//
//        let rec w (ps:List<float>) (phis:List<float>) (i:int) =
//            if ps.Length < phis.Length then 
//                let np = calcPJ ps phis 0.
//                w ((np / - (float i))::ps) phis (i + 1)
//            else
//                ps
//
//        let allPhis  = [ for i in [1..limit + 1] do yield (calcPhiL f i) ]
//        let allPhisM = [ for i in [1..limit + 1] do yield (calcPhiM f i) ]
//        let p0 = calcP0 f
//        let ps = w [p0] (allPhisM) 1
//        ps

