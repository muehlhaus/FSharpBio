namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharpBio")>]
[<assembly: AssemblyProductAttribute("FSharpBio")>]
[<assembly: AssemblyDescriptionAttribute("FsharpBio aims to be a user-friendly library for Bioinformatics written in F#.")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
