(*** hide ***)
#I "../../bin"



(**
Samples for FsharpBio Project core extensions
-----------------------------------------------

The core extensions 
 
*)


#r "FSharp.CoreX.dll"

open FSharp.CoreX
open Graph


type TNode = { 
    Id : int
    }
    with
    interface INode<int>
        with member this.Id = this.Id
    
    //override this.ToString() = sprintf "NodeId: %i" id


type TEdge(id,sourceId,targetId) =
    interface IEdge<int> with
        member this.Id = id
        member this.SourceId = sourceId
        member this.TargetId = targetId
    override this.ToString() = sprintf "Edge: %i from: %i to: %i" id sourceId targetId





let nA = { Id = 0 }
let nB = { Id = 1 }
let nC = { Id = 2 }
let e1 = TEdge(0,0,1)
let e2 = TEdge(1,1,0)
let e3 = TEdge(2,0,2)

let g : AdjacencyGraph<TNode,TEdge,int> = [(nA,[e1;e2]);(nB,[e1;e2]);(nC,[e3])]



//http://graphml.graphdrawing.org/primer/graphml-primer.html

let nodeToGraphML (node:#INode<'Tkey>) =
    sprintf "<node id=\"%A\"></node>" node.Id 


let edgeToGraphML (edge:#IEdge<'Tkey>) =
    sprintf "<edge id=\"%A\" source=\"%A\" target=\"%A\"></edge>" edge.Id edge.SourceId edge.TargetId


let Nodes = g |> List.map fst |> List.map nodeToGraphML
let Links = g |> List.collect snd |> List.map  edgeToGraphML