namespace FSharpBio

open FSharp.CoreX

module Helper =
    
    let splitCre (input) =
        match input with
            | Regex.RegexValue @"Cre(?<chromosom>[\d]*)\.(?<locusId>g[\d]*)\.t(?<spliceId>[\d]*)\.(?<version>[\d]*)" [ chromosom; locusId; spliceId; version; ] -> Some( chromosom, locusId, spliceId, version)
            | _ -> None

    
    let toCreLocusId  (input) =
        match input with
            | Regex.RegexValue @"Cre(?<chromosom>[\d]*)\.(?<locusId>g[\d]*)\.t(?<spliceId>[\d]*)\.(?<version>[\d]*)" [ chromosom; locusId; _ ; _; ] -> Some( sprintf "Cre%s.%s" chromosom locusId)
            | _ -> None