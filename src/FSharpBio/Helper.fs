namespace FSharpBio

open FSharp.CoreX

module Helper =
    
    let splitCre (input) =
        match input with
            | Regex.RegexValue @"Cre(?<chromosom>[\d]*)\.(?<locusId>g[\d]*)\.t(?<spliceId>[\d]*)\.(?<version>[\d]*)" [ chromosom; locusId; spliceId; version; ] -> Some( chromosom, locusId, spliceId, version)
            | _ -> None

    
    let toCreLocusId  (input) =
        match input with
            | Regex.RegexValue @"Cre(?<chromosom>[\d]*)\.g(?<locusId>[\d]*)\.t(?<spliceId>[\d]*)\.(?<version>[\d]*)" [ chromosom; locusId; _ ; _; ] -> Some( sprintf "Cre%s.g%s" chromosom locusId)
            | _ -> None



    let tryGetPfamId (str) =
        match str with
            | Regex.RegexValue @"Pfam:(?<pfamId>PF[\d]+)" [ pfamId; ] -> Some( pfamId )
            | _ -> None

    let tryGetGOId (str) =
        match str with
            | Regex.RegexValue @"(?<goId>GO:[\d]+)" [ goId; ] -> Some( goId )
            | _ -> None

