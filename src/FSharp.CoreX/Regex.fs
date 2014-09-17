namespace FSharp.CoreX

open System.Net
open System.Text.RegularExpressions


module Regex =

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

//    let matchExample =
//        match "input" with
//            | Regex @"Cre(?<chromosome>[\d]*)\.(?<locusId>g[\d]*)\.t(?<spliceId>[\d]*)\.(?<version>[\d]*)" [ chromosome; locusId; spliceId; version; ] -> Some( chromosome, locusId, spliceId, version)
//            | _ -> None

