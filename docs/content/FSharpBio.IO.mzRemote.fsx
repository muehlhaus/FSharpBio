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

let spectrumHeader = MzRemote.getSpectraHeaderContentAsync lg rawFileId 1250 1300 |> Async.RunSynchronously

let mzFragments    = MzRemote.getMZFragmentsAsync lg rawFileId "0_0_1296" |> Async.RunSynchronously |> Seq.filter (fun mzF -> mzF.Mass > 375. && mzF.Mass < 376. )





open FSharp.Charting

Chart.Point(mzFragments |> Seq.map (fun peak -> peak.Mass,peak.Intensity)) |> Chart.ShowChart



//centroid 
let byCenterOfGravity (minIntensity:float) (rawpeaks:seq<MzRemote.MZFragment>) =
    
    // Peak must be concave in the interval [i-2 .. i+2]
    let isConcave (minIntensity:float) c cm1 cm2 cp1 cp2 = 
        c > minIntensity && c > cm1 && c >= cp1 && cm1 > cm2 && cp1 >= cp2

    2.



