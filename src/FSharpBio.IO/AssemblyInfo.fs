namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharpBio.IO")>]
[<assembly: AssemblyProductAttribute("FSharpBio")>]
[<assembly: AssemblyDescriptionAttribute("FSharpBio aims to be a user-friendly library for Bioinformatics written in F#.")>]
[<assembly: AssemblyVersionAttribute("0.0.0")>]
[<assembly: AssemblyFileVersionAttribute("0.0.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.0"
