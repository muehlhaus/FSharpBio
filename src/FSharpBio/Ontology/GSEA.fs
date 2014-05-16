namespace FSharpBio.Ontology

open System
open System.Collections.Generic
open FSharp.CoreX

//open Microsoft.FSharp.Collections.Tagged


//// ---------------------------------------------------------------------------------------
// Gene Set Enrichment Analysis (GSEA) is a computational method that 
// determines whether an a priori defined set of genes shows statistically 
// significant, concordant differences between two biological states 
// (e.g. phenotypes). 
module GSEA =
    
    // #################################################
    // Ontology entry compares by displayId (uniqueness)
    type OntologyEntry
       (iD          : System.Guid,
        displayID   : string,
        binID       : string,
        group       : int
       ) = class
        
        let mutable _iD         = iD
        let mutable _displayID  = displayID
        let mutable _binID      = binID
        let mutable _group      = group
        
        override x.Equals(yobj) =
            match yobj with
            | :? OntologyEntry as y -> ((x.DisplayId = y.DisplayId)&& (x.TermId = y.TermId))
            | _ -> false


        override x.GetHashCode() = hash (x.DisplayId + x.TermId)


        (* empty constructor *)
        new () = OntologyEntry (    System.Guid.Empty,
                                    Unchecked.defaultof<string>,
                                    Unchecked.defaultof<string>,
                                    Unchecked.defaultof<int> )
        
        
        (* constructor *)
        new (id, displayID, binID) = OntologyEntry (id,displayID,binID,-1)            
        
               
        member this.Id              = _iD
        member this.DisplayId       = _displayID
        member this.TermId          = _binID
        member this.Group           = _group
    

    end

    //#########################
    // OntologyEntryDisplayTermidComparer Set wrapper
    type OntologyEntryDisplayTermidComparer = 
      interface IComparer<OntologyEntry> with
        member x.Compare(a, b) = ((a.DisplayId + a.TermId).CompareTo(b.DisplayId + b.TermId))         //(a.DisplayId).CompareTo(b.DisplayId) +
      new () = {}

    // Type alias for a set that uses 'OntologyEntryDisplayTermidComparer'
    //type OntologyEntrySet = Tagged.Set<OntologyEntry, OntologyEntryDisplayTermidComparer>


    // ###########################################################################################################
    // the hypergeometric distribution is a discrete probability distribution that describes the probability of 
    //   k successes in
    //   n draws from a finite 
    //   x population of size containing
    //   m successes without replacement (successes states)
    /// Calculates p value based on hypergeometric distribution (pValue <= k)
    let CalcHyperGeoPvalue numberOfDEsInBin numberInBin totalUnivers totalNumberOfDE (splitPvalueThreshold:int) =
        if (numberOfDEsInBin > 1) then
            let hp = MathNet.Numerics.Distributions.Hypergeometric(totalUnivers,totalNumberOfDE,numberInBin)
            if numberInBin > splitPvalueThreshold then                                
                // Calculate normal pValue
                (1. -  hp.CumulativeDistribution(float(numberOfDEsInBin + 1)) )
            else
                // Calculate split pValue
                0.5 * ((1. -  hp.CumulativeDistribution(float(numberOfDEsInBin + 1)) ) + ( (1. -  hp.CumulativeDistribution(float(numberOfDEsInBin))) ) )
        else
            nan 


    // #######################################################    
    //  functional term enrichment is calculated according to following publication
    //  http://bioinformatics.oxfordjournals.org/cgi/content/abstract/23/4/401
    //  also includes mid-pValues
    /// Calculates functional term enrichment
    let overEnrichment (deGroupIndex:int) (splitPvalueThreshold:option<int>) (minNumberInTerm:option<int>) (data:seq<OntologyEntry>) =
        let _splitPvalueThreshold   = defaultArg splitPvalueThreshold 5
        let _minNumberInTerm        = defaultArg minNumberInTerm 2
        
        // Distinct by term and gene name
        // Has to be done by an ouside function
        //let distinctData    = data |> Seq.distinctBy (fun o -> o.displayID)                
        let gData           = data |> Seq.groupBy ( fun o -> o.TermId)
        // reduce to terms at least annotated with 2 items
        let fData = gData |> Seq.filter ( fun (key:string,values:seq<OntologyEntry>) -> Seq.length(values) >= _minNumberInTerm)
        let groupCount = fData |> Seq.collect (fun (key:string,values:seq<OntologyEntry>) -> values ) |> Seq.countBy (fun o -> o.Group)
        
        let totalUnivers    = groupCount |> Seq.fold (fun  (acc:int) (index:int,count:int) -> acc + count) 0
        let totalNumberOfDE = 
            let tmp = groupCount |> Seq.tryFind (fun (key,v) -> key = deGroupIndex)
            if tmp.IsNone then 
                raise (System.ArgumentException("DE group index does not exists in ontology entry"))
            else
                snd(tmp.Value)
        
        // returns (DE count, all count)
        let countDE (subSet:seq<OntologyEntry>) =             
            let countMap = 
                subSet 
                |> Seq.countBy (fun (o:OntologyEntry) -> if o.Group = deGroupIndex then true else false )
                |> Map.ofSeq
            (countMap.TryFindDefault 0 true,(countMap.TryFindDefault 0 true) + (countMap.TryFindDefault 0 false))
        
        fData |> Seq.map (fun (key:string,values:seq<OntologyEntry>) ->  let numberOfDEsInBin,numberInBin = countDE values
                                                                         (key,(CalcHyperGeoPvalue numberOfDEsInBin numberInBin totalUnivers totalNumberOfDE _splitPvalueThreshold)) )

    /// Returns a enumerable sequence which is distinct by DisplayId and TermId
    let distinctByDisplayIdAndTermId (data:seq<OntologyEntry>) =
        data |> Seq.distinctBy (fun o -> (o.DisplayId,o.TermId))


    /// Splites OntologyEntry with conacat TermId
    /// Attention: Also parses string to int to get rit of 0 - terms
    let splitMapManOntologyItems (separator:char) (data:seq<OntologyEntry>) =
        let splitTerm (termId:string) (separator:char) =
            termId.Split(separator) 
            |> Array.map (fun sTerm -> let splited = sTerm.Split('.')
                                       let toInt = splited |> Seq.map (fun v -> //let _ = (printfn "- %s" (v.ToString()))
                                                                                Int32.Parse(v).ToString())
                                       toInt  |> String.concat "." )
        data
        |> Seq.collect (fun oi -> splitTerm oi.TermId separator
                                    |> Seq.map (fun sTerm -> OntologyEntry(oi.Id,oi.DisplayId,sTerm,oi.Group) )
                        )

    /// Extends leaf OntologyEntries to their full tree
    let expandOntologyTree (data:seq<OntologyEntry>) =
        data
        |> Seq.collect (fun oi -> let expandenTermIds = oi.TermId.Split('.') |> Array.scanReduce (fun acc elem -> acc + "." + elem)
                                  expandenTermIds |> Seq.map (fun sTerm -> OntologyEntry(oi.Id,oi.DisplayId,sTerm,oi.Group) )
                            )


    