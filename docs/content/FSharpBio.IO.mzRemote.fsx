(*** hide ***)
#I "../../bin"

(**



**)

// ##################################################################
// Examples for FsharpBio.IO.mzRemote
//
// --- Csv-reader
// --- FatsA reader/ writer
#r "FSharp.CoreX.dll"
#r "FSharp.Charting.dll"
#r "FSharpBio.dll"

#r "System.Net.Http"

open FSharpBio.IO.WebServices

let lg = FSharpBio.IO.ODataIO.loginAsync "http://iomiqsws1.bio.uni-kl.de:65000" "luede" "Luede123#" |> Async.RunSynchronously



let rawFiles        = MzRemote.getRawfilesContentAsync lg |> Async.RunSynchronously 

let rawFileId = rawFiles |> Seq.nth 2 |> fun r -> r.ID

let spectrumHeader = MzRemote.getSpectraHeaderContentAsync lg rawFileId 1 10 |> Async.RunSynchronously

let mzFragments    = MzRemote.getMZFragmentsAsync lg rawFileId "scan=1" |> Async.RunSynchronously


open FSharp.Charting

Chart.Point(mzFragments |> List.map (fun peak -> peak.Mass,peak.Intensity)) |> Chart.ShowChart
