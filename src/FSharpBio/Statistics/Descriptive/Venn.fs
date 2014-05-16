namespace FSharpBio.Statistics.Descriptive

// Set theory
module Venn =


    /// Venn with 2 Set (a;b)
    type Venn2n = { TotalSize             : int;
                    SizeA                 : int;
                    SizeB                 : int;              
                    Size_AintersectsB     : int;                                                            
                }


    /// Creates Venn with two Set (a;b)
    let private createVenn_2n total sizeA sizeB size_AintersectsB =
        { TotalSize = total; SizeA = sizeA; SizeB = sizeB; Size_AintersectsB = size_AintersectsB; }

    
    /// Calculates a Venn out of two given Sets
    let ofSetAB (a:Set<'a>) (b:Set<'a>) =
        // create Sets
        let total               = Set.unionMany ([a;b;])
        let setAintersectsB     = Set.intersect a b        
        // Counts    
        let distinctAintersectsB = setAintersectsB.Count        
        let distinctA = a.Count - setAintersectsB.Count
        let distinctB = b.Count - setAintersectsB.Count        

        createVenn_2n total.Count distinctA distinctB  distinctAintersectsB


    /// Venn with 3 Set (a;b;c)
    type Venn3n = { TotalSize             : int;
                    SizeA                 : int;
                    SizeB                 : int;
                    SizeC                 : int;
                    Size_AintersectsB     : int;
                    Size_AintersectsC     : int;
                    Size_BintersectsC     : int;
                    Size_ABC_intersection : int;
                }

    
    /// Creates Venn with 3 Set (a;b;c)
    let private createVenn_3n total sizeA sizeB sizeC size_AintersectsB size_AintersectsC size_BintersectsC size_ABC_intersection =
        { TotalSize = total; SizeA = sizeA; SizeB = sizeB; SizeC = sizeC; Size_AintersectsB = size_AintersectsB; Size_AintersectsC = size_AintersectsC; Size_BintersectsC = size_BintersectsC; Size_ABC_intersection = size_ABC_intersection}

    
    /// Calculates a Venn out of 3 given Sets
    let ofSetABC (a:Set<'a>) (b:Set<'a>) (c:Set<'a>) =
        // create Sets
        let total               = Set.unionMany ([a;b;c;])
        let setAintersectsB     = Set.intersect a b
        let setAintersectsC     = Set.intersect a c
        let setBintersectsC     = Set.intersect b c        
        let setABC_intersection = Set.intersect setAintersectsB c   
        // Counts    
        let distinctAintersectsB = setAintersectsB.Count - setABC_intersection.Count
        let distinctAintersectsC = setAintersectsC.Count - setABC_intersection.Count
        let distinctBintersectsC = setBintersectsC.Count - setABC_intersection.Count

        let distinctA = a.Count - setAintersectsB.Count - setAintersectsC.Count + setABC_intersection.Count
        let distinctB = b.Count - setAintersectsB.Count - setBintersectsC.Count + setABC_intersection.Count
        let distinctC = c.Count - setAintersectsC.Count - setBintersectsC.Count + setABC_intersection.Count

        createVenn_3n total.Count distinctA distinctB distinctC distinctAintersectsB distinctAintersectsC distinctBintersectsC setABC_intersection.Count    

