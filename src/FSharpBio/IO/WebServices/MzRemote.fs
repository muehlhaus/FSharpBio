namespace FSharpBio.IO.WebServices.MzRemote

module MzRemote =
    
    let a = 0

//    open System
//    open System.Net.Http    
//    open System.Collections.Generic
//    open FSharp.Data
//    open FSharp.Data.JsonExtensions   
//    open FSharpBio.Mz.Spectra
//    
//    type MZFragment = {RawFile:string; SpectrumID:string; PeakIndex:int; Mass:float; Intensity:float}    
//
//    let loginAsync(loginUri:string) (user:string) (password:string) =
//        async {
//            let requestHandler = new WebRequestHandler();
//            requestHandler.UseCookies <- true;
//            let client = new HttpClient(requestHandler)
//            client.BaseAddress <- new Uri(loginUri)
//            let loginData = [ new KeyValuePair<string, string>("UserName", user); new KeyValuePair<string, string>("Password", password)  ]
//            let content = new FormUrlEncodedContent(loginData);
//            let! response = client.PostAsync("/MzRemoteApi/Login", content) |> Async.AwaitTask
//            response.EnsureSuccessStatusCode() |> ignore
//            return client
//        }
//
//    let getAsync (httpClient:HttpClient) (url:string) = 
//        async {            
//            let! response = httpClient.GetAsync(url) |> Async.AwaitTask
//            response.EnsureSuccessStatusCode() |> ignore
//            let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
//            return content
//        }
//
//    // http://localhost:3391/OData/GetSpectrumHeaders(rawFileID=19e3ca87-e3d0-4509-bbc3-dbad065d7030)?$skip=0&$top=50&$expand=MZFragments,PrecursorInfo
//    let spectraHeaderODataUri(rawFileID:Guid) (skip:int) (top:int) =
//        sprintf "OData/GetSpectrumHeaders(rawFileID=%s)?$skip=%i&$top=%i&$expand=PrecursorInfo" (rawFileID.ToString()) skip top
//        
//    // http://localhost:3391/OData/GetMZData(rawFileID=19e3ca87-e3d0-4509-bbc3-dbad065d7030, spectrumID='0_0_0')
//    let mzDataODataUri(rawFileID:Guid) (spectrumID:string) =
//        sprintf "OData/GetMZData(rawFileID=%s,spectrumID='%s')" (rawFileID.ToString()) spectrumID
//
//    let getSpectraHeaderContentAsync(httpClient:HttpClient) (rawFileID:Guid) (skip:int) (top:int) =
//        async {            
//            let uri = spectraHeaderODataUri rawFileID skip top
//            let! content = getAsync httpClient uri 
//            return content             
//        }
//
//    let getMZDataContentAsync(httpClient:HttpClient) (rawFileID:Guid) (spectrumID:string) =
//        async {            
//            let uri = mzDataODataUri rawFileID spectrumID
//            let! content = getAsync httpClient uri 
//            return content             
//        }
//
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
//    type WebReaderCredentials = {UserName:string; Password:string}
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
