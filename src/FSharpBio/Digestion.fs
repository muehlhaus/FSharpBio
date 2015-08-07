namespace FSharpBio

module Digestion =
    
    open AminoAcids

    // p4 p3 p2 p1 || p1' p2'
    type Protease = {
        Name : string
        Expression : AminoAcid option -> AminoAcid option
                  -> AminoAcid option -> AminoAcid option
                  -> AminoAcid option-> AminoAcid option -> bool    
        }

    let createProtease name f =
        {Name = name; Expression = f}

    let trypsin = createProtease "Trypsin" 
                        (let _p1 = [AminoAcid.Lys;AminoAcid.Arg] |> Set.ofList 
                         fun p4 p3 p2 p1 p1' p2' -> 
                            match p1,p1' with
                            | Some a1,Some a1' -> _p1.Contains(a1) && not (a1' = AminoAcid.Pro)
                            | _   -> false                     
                         )

    let isCutingSite (protease:Protease) (arr:AminoAcid option[]) =
        match arr with
        | [|p4; p3; p2; p1; p1'; p2';|] -> protease.Expression p4 p3 p2 p1 p1' p2'
        | _ -> false

    /// Returns current value,array tuple (current, [|prefix; current; suffix)
    let motivy prefixLength suffixLength (source: seq<'T>) =    
        if prefixLength < 0 then invalidArg "prefixLength" "Input must be non negative"
        if suffixLength < 0 then invalidArg "suffixLength" "Input must be non negative"
        let windowSize = prefixLength + suffixLength + 1
        //if windowSize <= 0 then invalidArg "windowSize" "Input must be non zero"
    
        seq {   let arr = Array.create windowSize None
                let r = ref (suffixLength ) 
                let i = ref (prefixLength) 
                use e = source.GetEnumerator()
                while e.MoveNext() do
                    arr.[!i] <- Some e.Current   // ! get while := set
                    i := (!i + 1) % windowSize
                    if !r = 0 then
                        let tmp = Array.init windowSize (fun j -> arr.[(!i+j) % windowSize])
                        yield (tmp.[prefixLength].Value,tmp)
                    else
                    r := (!r - 1) 
                // continue shifting for suffixLength  
                let arr = Array.init windowSize (fun j -> arr.[(!i+j) % windowSize])
                for i = 1 to suffixLength do
                    let tmp = Array.create windowSize None
                    Array.blit arr i tmp 0 (arr.Length-i)
                    yield (tmp.[prefixLength].Value,tmp)
                    }

    /// 
    let digest (protease:Protease) (aas:seq<AminoAcid>) =

        let groupAfter f (input:seq<_>) =     
            let rec group (en:System.Collections.Generic.IEnumerator<_>) cont acc c  =            
                    if not(f en.Current) && en.MoveNext() then
                        group en (fun l -> cont <| c::l) acc (fst en.Current) // modified!
                    else
                        (fun l -> cont <| c::l) []
            seq{
                use en = input.GetEnumerator()
                while en.MoveNext() do
                    yield group en id [] (fst en.Current) }// modified! 

        aas
        |> motivy 3 2
        |> groupAfter (fun (c,arr) -> isCutingSite protease arr)


