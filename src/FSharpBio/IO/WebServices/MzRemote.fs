namespace FSharpBio.IO.WebServices


module MzRemote =

    open System
    open System.Net.Http
    open FSharpBio.IO.ODataIO
       



    type WebReaderCredentials = {UserName:string; Password:string}

    type RawFile = 
        { ID : System.Guid
          Name : string
          path : string
          UserID : string
          RawFileFormat : string
          DateCreated : DateTime      
          RawHash : System.Guid
        }

    
    //type MZFragment = {RawFile:string; SpectrumID:string; PeakIndex:int; Mass:float; Intensity:float}    
    type MZFragment = { Mass:float; Intensity:float}    

    type PrecursorInfo =
        {  IsolationWindowTargetMz : float
           IsolationWindowLowerOffset : float
           IsolationWindowUpperOffset : float
           DissociationMethod : string
           ChargeState : int
        }

    type SpectumHeader =
        { RawFile                : string
          SpectrumID             : string
          SpectrumRepresentation : string
          MSLevel                : int
          ScanPolarity           : string
          RetentionTime          : float
          PrecursorInfo          : PrecursorInfo
        }


    // #baseUri#/OData/RawFiles
    let private rawFilesUri = "OData/RawFiles"

    // #baseUri#/OData/GetSpectrumHeaders(rawFileID=19e3ca87-e3d0-4509-bbc3-dbad065d7030)?$skip=0&$top=50&$expand=MZFragments,PrecursorInfo
    let private spectraHeaderODataUri(rawFileID:Guid) (skip:int) (top:int) =
        sprintf "OData/GetSpectrumHeaders(rawFileID=%s)?$skip=%i&$top=%i&$expand=PrecursorInfo" (rawFileID.ToString()) skip top
        
    // #baseUri#/OData/GetMZData(rawFileID=19e3ca87-e3d0-4509-bbc3-dbad065d7030, spectrumID='0_0_0')
    let private mzFragmentDataODataUri(rawFileID:Guid) (spectrumID:string) =
        sprintf "OData/GetMZData(rawFileID=%s,spectrumID='%s')" (rawFileID.ToString()) spectrumID

    // #baseUri#/OData/GetMZArray(rawFileID=19e3ca87-e3d0-4509-bbc3-dbad065d7030, spectrumID='0_0_0')
    let private mzArrayODataUri(rawFileID:Guid) (spectrumID:string) =
        sprintf "OData/GetMZArray(rawFileID=%s,spectrumID='%s')" (rawFileID.ToString()) spectrumID

    // #baseUri#/OData/GetMZArrayByMassRange(rawFileID=19e3ca87-e3d0-4509-bbc3-dbad065d7030, spectrumID='0_0_0',lowMass=500, highMass=501)
    let private mzArrayByMassRangeODataUri(rawFileID:Guid) (spectrumID:string) (lowMass:int)  (highMass:int) =
        sprintf "OData/GetMZArrayByMassRange(rawFileID=%A,spectrumID='%s',lowMass=%i,highMass=%i)" rawFileID spectrumID lowMass highMass



    
    /// Gets 
    let getRawfilesContentAsync(httpClient:HttpClient) =
        async {            
            let uri = rawFilesUri
            let! content = getODataResponseAsync<list<RawFile>> httpClient uri 
            return content.Value             
        }


    let getSpectraHeaderContentAsync(httpClient:HttpClient) (rawFileID:Guid) (skip:int) (top:int) =
        async {            
            let uri = spectraHeaderODataUri rawFileID skip top
            let! content = getODataResponseAsync<list<SpectumHeader>> httpClient uri 
            return content.Value             
        }



    let getMZFragmentsAsync(httpClient:HttpClient) (rawFileID:Guid) (spectrumID:string) =
        async {            
            let uri = mzFragmentDataODataUri rawFileID spectrumID
            let! content = getODataResponseAsync<list<MZFragment>> httpClient uri 
            return content.Value
        }



    let getMzArrayByMassRangeAsync(httpClient:HttpClient) (rawFileID:Guid) (spectrumID:string) (lowMass:int)  (highMass:int) =
        async {            
            let uri = mzArrayByMassRangeODataUri rawFileID spectrumID lowMass highMass
            let! content = getODataResponseAsync<array<float>> httpClient uri 
            return content.Value
        }


    let getMzArrayAsync(httpClient:HttpClient) (rawFileID:Guid) (spectrumID:string) =
        async {            
            let uri = mzArrayODataUri rawFileID spectrumID
            let! content = getODataResponseAsync<array<float>> httpClient uri 
            return content.Value
        }

    let private fromShrinked (arr:float[]) =     
        Array.init (arr.Length/2) (fun i -> arr.[i*2],arr.[i*2+1])

    let getMzByMassRangeAsync(httpClient:HttpClient) (rawFileID:Guid) (spectrumID:string) (lowMass:int)  (highMass:int) =
        async {            
            let uri = mzArrayByMassRangeODataUri rawFileID spectrumID lowMass highMass
            let! content = getODataResponseAsync<array<float>> httpClient uri 
            return fromShrinked content.Value
        }


    let getMzAsync(httpClient:HttpClient) (rawFileID:Guid) (spectrumID:string) =
        async {            
            let uri = mzArrayODataUri rawFileID spectrumID
            let! content = getODataResponseAsync<array<float>> httpClient uri 
            return fromShrinked content.Value
        }







/// +++++++++++++++++++++++
// ---> old

//    let createPrecursorInfo(record:JsonValue) =
//        match record with 
//        | JsonValue.Null -> Unchecked.defaultof<PrecursorInfo>
//        | _ ->  {  IsolationWindowTargetMz = (record?IsolationWindowTargetMz).AsFloat(); 
//                    IsolationWindowLowerOffset = (record?IsolationWindowLowerOffset).AsFloat(); 
//                    IsolationWindowUpperOffset = (record?IsolationWindowUpperOffset).AsFloat(); 
//                    DissociationMethod = (record?DissociationMethod).AsString(); 
//                    ChargeState = (record?ChargeState).AsInteger(); 
//                }
//
//    let createSpectumHeader(record:JsonValue) =
//        {  RawFile = (record?RawFileID).AsString(); 
//            SpectrumID = (record?SpectrumID).AsString(); 
//            SpectrumRepresentation = (record?SpectrumRepresentation).AsString(); 
//            MSLevel = (record?MSLevel).AsInteger(); 
//            ScanPolarity = (record?ScanPolarity).AsString(); 
//            RetentionTime = (record?RetentionTime).AsFloat(); 
//            PrecursorInfo = createPrecursorInfo record?PrecursorInfo;
//         }
//
//    let createMZFragment(record:JsonValue) =
//        {
//            RawFile = (record?RawFileID).AsString(); 
//            SpectrumID = (record?SpectrumID).AsString(); 
//            PeakIndex = (record?PeakIndex).AsInteger(); 
//            Mass = (record?Mass).AsFloat();
//            Intensity = (record?Intensity).AsFloat();
//        }
//
//    let getMZFragments(httpClient:HttpClient) (rawFileID:Guid) (spectrumID:string) =
//        seq {        
//            let content = getMZDataContentAsync httpClient rawFileID spectrumID |> Async.RunSynchronously
//            let parsed = JsonValue.Parse(content)
//            match parsed with
//            | JsonValue.Record [| odata; value |] -> 
//                match value with
//                | (xxx, array) -> 
//                    for record in array do     
//                        if record <> JsonValue.Null then                                          
//                            yield createMZFragment record                
//            | _ -> raise (System.FormatException("Error parsing json content."))
//        }
//
//    let getSpectraHeader(httpClient:HttpClient) (rawFileID:Guid) (skip:int) (top:int) =
//        seq {
//            let content = getSpectraHeaderContentAsync httpClient rawFileID skip top |> Async.RunSynchronously
//            let parsed = JsonValue.Parse(content)
//            match parsed with
//            | JsonValue.Record [| odata; value |] -> 
//                match value with
//                | (xxx, array) -> 
//                    for record in array do     
//                        if record <> JsonValue.Null then                                          
//                            yield createSpectumHeader record
//            | _ -> raise (System.FormatException("Error parsing json content."))
//        }
//
//    
//    let getAllPages(httpClient:HttpClient) (rawFileID:Guid) (pageSize:int) =         
//        seq {
//            let sourceIsEmpty = ref false            
//            let skip = ref 0
//
//            while (!sourceIsEmpty) = false do
//                let results = getSpectraHeader httpClient rawFileID (!skip) pageSize |> Seq.toArray
//                if(results.Length > 0) then
//                    yield! results
//                    skip := (!skip) + pageSize
//                else
//                    sourceIsEmpty := true;
//
//        }
//
//    
//
//    type WebRawFileReader(baseUri:string, rawFileID:Guid, credentials:WebReaderCredentials) = class 
//        let mutable httpClient = Unchecked.defaultof<HttpClient>        
//        let mutable isInitial = true
//        let mutable current = Unchecked.defaultof<SpectrumHeader>
//        let mutable enumerator = Unchecked.defaultof<IEnumerator<SpectrumHeader>>
//        let pageSize = 50        
//
//        member this.PageSize with get() = pageSize
//        member this.BaseUri with get() = baseUri
//        member this.RawFileID with get() = rawFileID
//        member this.Credentials with get() = credentials         
//
//        member this.MoveNext() =
//            if(isInitial) then
//                httpClient <- loginAsync baseUri  this.Credentials.UserName this.Credentials.Password |> Async.RunSynchronously                
//                let pages = getAllPages httpClient rawFileID pageSize
//                enumerator <- pages.GetEnumerator()
//                isInitial <- false
//
//            if(enumerator.MoveNext()) then
//                current <- enumerator.Current
//                true
//            else
//                httpClient <- Unchecked.defaultof<HttpClient>
//                current <- Unchecked.defaultof<SpectrumHeader>
//                enumerator <- Unchecked.defaultof<IEnumerator<SpectrumHeader>>
//                isInitial <- true
//                false
//
//        member this.Current() = 
//            if(isInitial) then
//                raise (System.InvalidOperationException("Reader is in initial state. Please call MoveNext() first."))
//            else
//                current
//
//        member this.CurrentMZ() = 
//            if(isInitial) then
//                raise (System.InvalidOperationException("Reader is in initial state. Please call MoveNext() first."))
//            else
//                getMZFragments httpClient rawFileID current.SpectrumID |> Seq.toArray
//
//        static member GetScanMS2(baseUri:string, rawFileID:Guid, credentials:WebReaderCredentials) =
//            let reader = new WebRawFileReader(baseUri, rawFileID, credentials)
//            let spectrumOfMSMSDataFragments totalIntensity (currentMZ: seq<MZFragment>) = 
//                currentMZ 
//                |> Seq.map (fun fragment -> fragment.Mass,fragment.Intensity)
//                |> Seq.map (fun (m,i)    -> createPeak IonTypes.Unknown m i (i/totalIntensity))
//
//            let totalIntensity (currentMZ: seq<MZFragment>) =
//                currentMZ |> Seq.sumBy (fun x -> x.Intensity)
//
//            seq {
//                while reader.MoveNext() do
//                    let sh = reader.Current()
//                    if(sh.MSLevel >= 2) then
//                        let frag = reader.CurrentMZ()
//                        let total = totalIntensity frag
//                        let spec = spectrumOfMSMSDataFragments total frag
//                        yield createScan spec sh
//            }
//
//    end
//
