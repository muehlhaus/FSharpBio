namespace FSharp.CoreX


module Dictionary = 
    
    let a = 0
//    open System.Collections.Generic
//
//      let toSeq d = d |> Seq.map (fun (KeyValue(k,v)) -> (k,v))
//  let toArray (d:IDictionary<_,_>) = d |> toSeq |> Seq.toArray
//  let toList (d:IDictionary<_,_>) = d |> toSeq |> Seq.toList
//  let ofMap (m:Map<'k,'v>) = new Dictionary<'k,'v>(m) :> IDictionary<'k,'v>
//  let ofList (l:('k * 'v) list) = new Dictionary<'k,'v>(l |> Map.ofList) :> IDictionary<'k,'v>
//  let ofSeq (s:('k * 'v) seq) = new Dictionary<'k,'v>(s |> Map.ofSeq) :> IDictionary<'k,'v>
//  let ofArray (a:('k * 'v) []) = new Dictionary<'k,'v>(a |> Map.ofArray) :> IDictionary<'k,'v>
//
//    // From fslib-extra-pervasives.fs
//    let dict l = 
//        // Use a dictionary (this requires hashing and equality on the key type)
//        // Wrap keys in an Some(_) option in case they are null 
//        // (when System.Collections.Generic.Dictionary fails). Sad but true.
//        let t = new Dictionary<Option<_>,_>(HashIdentity.Structural)
//        for (k,v) in l do 
//            t.[Some(k)] <- v
//        let d = (t :> IDictionary<_,_>)
//        let c = (t :> ICollection<_>)
//        let ieg = (t :> IEnumerable<_>)
//        let ie = (t :> System.Collections.IEnumerable)
//        // Give a read-only view of the dictionary
//        { new IDictionary<'key, 'a> with 
//                member s.Item 
//                    with get x = d.[Some(x)]            
//                    and  set (x,v) = raise (NotSupportedException(
//                                                "This value may not be mutated"))
