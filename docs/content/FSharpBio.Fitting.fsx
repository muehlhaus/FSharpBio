(*** hide ***)
#I "../../bin"


//#r @"D:\Development\FSharp\FSharpBio\bin\..
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.Fsharp.dll"
#r "FSharp.CoreX.dll"
#r "FSharpBio.dll"
#r "FSharp.Charting.dll"

open FSharp.Charting
open FSharpBio.Statistics.Fitting





let rnd = System.Random()
let rand = rnd.NextDouble
let data = [|for i in 1. .. 50. do yield i, i*(1.+3.*rand()+5.*(sin((i+3.*rand())/50.))) - (max (i-50.) 0.)*(max (i-50.) 0.)/5.|];;
let smg = Spline.smoothingSpline data [| 4. .. 45.|]


let tuple a b = (a,b)
[0.;0.1;3.]
|> List.map (fun lambda -> let spline = smg lambda
                           data |> Array.map (fun (x,_) -> x, spline x)
                                |> fun d -> Chart.Line(d,Name="Spline w lambda=" + string lambda))
|> tuple (Chart.Point data)
|> List.Cons
|> Chart.Combine
|> Chart.ShowChart





