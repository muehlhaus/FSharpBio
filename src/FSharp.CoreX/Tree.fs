namespace FSharp.CoreX

module Tree =
    

    /// Generic tree node containing member list and deep member list and child
    type GenericNode<'a, 'b when 'a : comparison> = { members : List<'b>; deepMembers : List<'b>; child : Map<'a, GenericNode<'a,'b>> }

    /// Generic tree node empty
    let empty = { members = [] ; deepMembers = []; child = Map.empty }


    /// Add item to tree having keyPath as list of keys
    let rec add keyPath item tree =
        match keyPath with
        | [] -> { tree with members = item::tree.members; deepMembers = item::tree.deepMembers}
        | k::key -> 
                let tree' =
                    match Map.tryFind k tree.child with
                    | Some tree' -> tree'
                    | None -> empty
                {tree with  deepMembers = (item::tree.deepMembers); child = Map.add k (add key item tree') tree.child }


    /// Create tree out of keyPath*item list 
    let createTree keyItemList = 
        List.foldBack (fun (elem1,elem2) acc -> add elem1 elem2 acc ) keyItemList empty

    /// Contains 
    let rec contains keyPath item tree = 
        match keyPath with
        | [] -> List.exists(fun i -> i = item) tree.members
        | k::key ->             
            match Map.tryFind k tree.child with
            | Some tree -> contains key item tree
            | None -> false

    /// returns the deep members of a path
    let rec getDeepMembers keyPath tree = 
        match keyPath with
        | [] -> tree.deepMembers
        | k::key ->             
            match Map.tryFind k tree.child with
            | Some tree -> getDeepMembers key tree
            | None -> []
    
    /// Converts tree to seq and returns only last deepMember or if deepMemerb count is <= maxMemberCount
    let rec toSeqFilter (tree) (maxMemberCount:int) = 
        seq {   for child in tree.child do
                    let con = child.Value.deepMembers.Length <= maxMemberCount || child.Value.child.IsEmpty
                    yield (if con then Some(child.Value.deepMembers) else None)
                    if not(con) then yield! toSeqFilter child.Value maxMemberCount
            }
