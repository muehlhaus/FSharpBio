namespace FSharp.CoreX

open Microsoft.FSharp.Core.OptimizedClosures

[<AutoOpen>]
module List = 
    
    let scanReduce f l = 
        match l with 
        | [] -> invalidArg "l" "the input list is empty"
        | (h::t) -> List.scan f h t

    let scanArraySubRight<'T,'State> (f:FSharpFunc<'T,'State,'State>) (arr:_[]) start fin initState = 
        let mutable state = initState  
        let mutable res = [state]  
        for i = fin downto start do
            state <- f.Invoke(arr.[i], state);
            res <- state :: res
        res

    let scanReduceBack f l = 
        match l with 
        | [] -> invalidArg "l" "the input list is empty"
        | _ -> 
            let f = FSharpFunc<_,_,_>.Adapt(f)
            let arr = Array.ofList l 
            let arrn = Array.length arr 
            scanArraySubRight f arr 0 (arrn - 2) arr.[arrn - 1]

    
    /// Cuts a list after N and returns both parts
    let cutAfterN n input = // Tail-recursive
        let rec gencut cur acc = function
            | hd::tl when cur < n ->
                gencut (cur+1) (hd::acc) tl
            | rest -> (List.rev acc), rest //need to reverse accumulator!
        gencut 0 [] input

//    // Non-tail-recursive
//    let cutAfterN n input =
//        let rec gencut cur = function
//            | hd::tl when cur < n ->
//                let x, y = gencut (cur+1) tl //stackoverflow with big lists!
//                hd::x, y
//            | rest -> [], rest
//        gencut 0 input

    /// Cuts a list into two parts
    let cut input = 
        let half = (input |> List.length) / 2
        input |> cutAfterN half



    /// Groups elements in list that are b function f
    let groupEquals f (input:'a list) =
        let rec groupLoop (first) (heap:'a list) (stack:'a list) (groupStack:'a list) =
            match heap with 
            | head::rest -> if (f first head) then
                                groupLoop first rest stack (head::groupStack)
                            else
                                groupLoop first rest (head::stack) groupStack
                        
            | []         -> stack,groupStack
    



        let rec outerLoop (heap:'a list) (stack:list<'a list>) =
            match heap with 
            | head::rest -> let filteredStack,groupStack = groupLoop head rest [] [head]
                            outerLoop filteredStack (groupStack::stack)
            | []         -> stack

    

        outerLoop input []


// ########################################
// Static extensions


[<AutoOpen>]
module FsharpListExtensions =
  

    //-------------->
    //List extensions

    type Microsoft.FSharp.Collections.List<'a > with //when 'a : equality
    
        //
        static member iterWhile (f:'a -> bool) (ls:'a list) = 
            let rec iterLoop f ls = 
                match ls with
                | head :: tail -> if f head then iterLoop f tail
                | _ -> ()
            iterLoop f ls


