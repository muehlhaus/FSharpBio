namespace FSharp.CoreX

open System.Net
open System.Text.RegularExpressions


module Regex =

    //http://stackoverflow.com/questions/5684014/f-mapping-regular-expression-matches-with-active-patterns

    // Returns the matching group of the first occurence of the pattern 
    let (|FirstRegexGroup|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g ])
        else None

    // Returns the matching group of all occurences of the pattern
    let (|RegexGroups|_|) pattern input =
        let m = Regex.Matches(input, pattern)
        if m.Count > 0 then Some([ for m' in m -> [for g in m'.Groups -> g] ])
        else None


    /// Returns the matching values the first occurence of the pattern 
    let (|RegexValue|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    /// Returns the matching values of all occurences of the pattern
    let (|RegexValues|_|) pattern input =
        let m = Regex.Matches(input, pattern)
        if m.Count > 0 then Some([ for g in m -> g.Value ])
        else None

//    let matchExample =
//        match "input" with
//            | RegexValue @"Cre(?<chromosome>[\d]*)\.(?<locusId>g[\d]*)\.t(?<spliceId>[\d]*)\.(?<version>[\d]*)" [ chromosome; locusId; spliceId; version; ] -> Some( chromosome, locusId, spliceId, version)
//            | _ -> None



