namespace FSharpBio.IO.WebServices.WSTargetP

module WSTargetP =

    open FSharpBio.IO.WebServices
    open System.Xml.Serialization
    open System.Runtime.Serialization
    open System.IO
    open System.Xml
    open System.Net

    // ws target request types, must attributed as XmlTypes

    [<CLIMutable>]
    [<XmlType(TypeName="sequence",Namespace ="http://www.cbs.dtu.dk/ws/WSTargetP_1_1_ws0")>]
    type Sequence = { 
        [<XmlElement(ElementName="id")>]
        ID:string;
        [<XmlElement(ElementName="comment")>]
        Comment:string;
        [<XmlElement(ElementName="seq")>]
        Sequence:string }

    [<CLIMutable>]   
    [<XmlType(TypeName="parameters",Namespace ="http://www.cbs.dtu.dk/ws/WSTargetP_1_1_ws0")>]
    type Parameters = {        
        [<XmlElement(ElementName="organism")>]
        Organism:string;
        [<XmlElement(ElementName="cleavage")>]
        Cleavage:string;
        [<XmlElement(ElementName="cTP")>]
        cTP:string;
        [<XmlElement(ElementName="SP")>]
        SP:string;
        [<XmlElement(ElementName="mTP")>]
        mTP:string;
        [<XmlElement(ElementName="other")>]
        Other:string;
        [<XmlArray(ElementName="sequencedata")>]    
        SequenceData: Sequence array }
    
    /// request type RunService, gets RunServiceResponse type
    [<CLIMutable>] 
    [<XmlType(TypeName="runService", Namespace ="http://www.cbs.dtu.dk/ws/WSTargetP_1_1_ws0")>]
    type RunService = { 
        [<XmlElement(ElementName="parameters")>]
        Parameters:Parameters }
 
    let createRun id comment sequence organism cleavage (cTP:float) (sP:float) (mTP:float) (other:float) =
        let formatFloat(value:float) = 
            let abs = System.Math.Abs value
            let floor = System.Math.Floor abs
            if(floor = abs) then
                System.String.Format("{0:F1}", value)
            else
                value.ToString()
        let testSequence = Seq.toArray [ { ID=id; Comment= comment; Sequence=sequence; }]
        let testParams = { Organism=organism; Cleavage=cleavage; cTP=formatFloat cTP; SP=formatFloat sP; mTP=formatFloat mTP; Other=formatFloat other; SequenceData=testSequence}
        {Parameters=testParams}

    [<CLIMutable>] 
    [<XmlType(TypeName="job", Namespace ="http://www.cbs.dtu.dk/ws/WSTargetP_1_1_ws0")>]
    type Job = {
        [<XmlElement(ElementName="jobid")>]
        JobID:string
        }

    /// request type PollQueue, gets PollQueueResponse type
    [<CLIMutable>] 
    [<XmlType(TypeName="pollQueue", Namespace ="http://www.cbs.dtu.dk/ws/WSTargetP_1_1_ws0")>]
    type PollQueue = {
        [<XmlElement(ElementName="job")>]
        Job:Job
        }

    let createPollQueue jobid = {Job={JobID=jobid}}

    [<CLIMutable>] 
    [<XmlType(TypeName="fetchResult", Namespace ="http://www.cbs.dtu.dk/ws/WSTargetP_1_1_ws0")>]
    type FetchResult = {
        [<XmlElement(ElementName="job")>]
        Job:Job
        }

    let createFetchResult jobid = {Job={JobID=jobid}}

    // ws target response types, must attributed as SoapTypes

    [<CLIMutable>]
    [<SoapType(TypeName="queueentry", Namespace ="http://www.cbs.dtu.dk/ws/WSTargetP_1_1_ws0")>]
    type QueueEntry = {
        [<SoapElement(ElementName="jobid")>]
        JobID:string;
        [<SoapElement(ElementName="datetime")>]
        DateTime:string;
        [<SoapElement(ElementName="status")>]
        Status:string;
        [<SoapElement(ElementName="expires")>]
        Expires:string;
    }

    /// response type of RunService
    [<CLIMutable>]  
    [<SoapType(TypeName="runServiceResponse", Namespace ="http://www.cbs.dtu.dk/ws/WSTargetP_1_1_ws0")>]  
    type RunServiceResponse = {
        [<SoapElement(ElementName="queueentry")>]
        QueueEntry:QueueEntry
    }

    /// response type of PollQueue
    [<CLIMutable>]  
    [<SoapType(TypeName="pollQueueResponse", Namespace ="http://www.cbs.dtu.dk/ws/WSTargetP_1_1_ws0")>]  
    type PollQueueResponse = {
        [<SoapElement(ElementName="queueentry")>]
        QueueEntry:QueueEntry
    }    

    let serviceURL = "http://ws.cbs.dtu.dk:80/cgi-bin/soap/ws/quasi.cgi?log"
    let tpHeadersRunService  = [  "Accept-Encoding", "gzip,deflate";
                              "Content-Type", "text/xml;charset=UTF-8";
                              "SOAPAction", "http://www.cbs.dtu.dk/ws/WSTargetP_1_1_ws0#runService";
                              "Host", "ws.cbs.dtu.dk:80";
                              "User-Agent", "Apache-HttpClient/4.1.1 (java 1.5)"; ]
    
    let tpHeadersPollQueue  = [  "Accept-Encoding", "gzip,deflate";
                              "Content-Type", "text/xml;charset=UTF-8";
                              "SOAPAction", "http://www.cbs.dtu.dk/ws/WSTargetP_1_1_ws0#pollQueue";
                              "Host", "ws.cbs.dtu.dk:80";
                              "User-Agent", "Apache-HttpClient/4.1.1 (java 1.5)"; ]

    let tpHeadersFetchResult  = [  "Accept-Encoding", "gzip,deflate";
                                  "Content-Type", "text/xml;charset=UTF-8";
                                  "SOAPAction", "http://www.cbs.dtu.dk/ws/WSTargetP_1_1_ws0#fetchResult";
                                  "Host", "ws.cbs.dtu.dk:80";
                                  "User-Agent", "Apache-HttpClient/4.1.1 (java 1.5)"; ]

    let runService(rs:RunService, verbose:bool) =
        let msg = SoapUtil.createSoapRequest rs        
        SoapUtil.getResponse<RunService, RunServiceResponse>(msg, serviceURL, tpHeadersRunService,verbose)
        
    let pollQueue(rsr:RunServiceResponse, verbose:bool) =
        let pq = createPollQueue rsr.QueueEntry.JobID
        let msg = SoapUtil.createSoapRequest pq        
        SoapUtil.getResponse<PollQueue, PollQueueResponse>(msg, serviceURL, tpHeadersPollQueue,verbose)     

    let fetchResult(pqr:PollQueueResponse, verbose:bool) = 
        let fr = createFetchResult pqr.QueueEntry.JobID 
        let msg = SoapUtil.createSoapRequest fr 
        SoapUtil.getResponse<FetchResult, string>(msg, serviceURL, tpHeadersFetchResult,verbose)    

    let runAll(rs:RunService, verbose:bool) =
        
        let pollQueueLocal(rsr:RunServiceResponse, pollSleep:int, verbose:bool) = 

            let mutable status = System.String.Empty
            let mutable pollResponse = Unchecked.defaultof<PollQueueResponse>

            while status.Equals("FINISHED") = false do

                System.Threading.Thread.Sleep(1000)
        
                let res2 = pollQueue(rsr, verbose)
                pollResponse <- match res2 with
                                | SoapUtil.Body d -> d.Body         
                                | SoapUtil.Fault f -> raise( new System.Net.WebException(f.FaultString)) 

                status <- pollResponse.QueueEntry.Status

            pollResponse

        let runSvcResponse = match runService(rs, verbose) with
                                | SoapUtil.Body d -> d.Body         
                                | SoapUtil.Fault f -> raise( new System.Net.WebException(f.FaultString)) 
                
        let pollResponse = pollQueueLocal(runSvcResponse, 1000, verbose)    
                         
        match fetchResult(pollResponse, verbose) with
            | SoapUtil.Body d -> d.Body         
            | SoapUtil.Fault f -> raise( new System.Net.WebException(f.FaultString)) 