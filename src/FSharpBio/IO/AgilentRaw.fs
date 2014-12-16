namespace FSharpBio.IO

open System
open FSharp.CoreX

open FSharpBio.IO
open FSharpBio.IO.SchemaReader
open FSharpBio.IO.SchemaReader.Csv
open FSharpBio.IO.SchemaReader.Attribute
   

module AgilentRaw =
    
    
    type GalProbeDescription = {
        //The block number for the feture
        [<FieldAttribute("Block")>]    Block        : int;
        [<FieldAttribute("Row")>]      Row          : int;
        [<FieldAttribute("Column")>]   Column       : int;
        [<FieldAttribute("ID")>]       ID           : string;
        [<FieldAttribute("GeneName")>] GeneName     : string;
        //[<FieldAttribute(8)>] Identscore   : float
        }

    let createGalProbeDescription block row column id genename identscore =
        { Block = block; Row = row; Column = column; ID = id; GeneName = genename; }






    let readGalProbeDescription separator hasHeader path = 
        let reader = new CsvReader<GalProbeDescription>(schemaMode=SchemaMode.Fill)
        reader.ReadFile(path, separator, hasHeader) |> Seq.toList

    let galProbeDescriptiontoMapping (input: GalProbeDescription seq) =
        input
        |> Seq.map (fun x -> (x.ID,x.GeneName))
        |> Map.ofSeq
    
    /// Record type representing microarray probe mapping
    type ProbeMapping = {
        [<FieldAttribute(0)>] ProbeName      : string;
        [<FieldAttribute(1)>] GeneName       : string         
        }


    /// Reads probe mapping. Map: probe name -> gene name 
    let probeMappingReader separator hasHeader path =
        let reader = new CsvReader<ProbeMapping>()
        reader.ReadFile(path, separator, hasHeader)
        |> Seq.map (fun (x:ProbeMapping) -> (x.ProbeName,x.GeneName))
        |> Map.ofSeq







    type AgilentControlType =
    | None = 0
    | PositiveControl = 1
    | NegativeControl = -1
    | DeletionControl = -10000
    | NotProbe = -2000


    type AgilentControlTypeConverter() = 
        inherit ConverterAttribute()
        override this.convertToObj = 
            Converter.Single(fun (str : string) -> Enum.Parse(typeof<AgilentControlType>, str) |> box )
                            

    //http://www.molmine.com/magma/loading/agilentSignals.html
    /// Record type representing microarray Agilent raw data item 
    type AgilentDataRaw = {        
        /// Feature number
        [<FieldAttribute("FeatureNum")>] FeatureNum : int
        ///  Feature location : row
        [<FieldAttribute("Row")>] Row : int
        /// Feature location : column
        [<FieldAttribute("Col")>] Col : int
        /// 
        [<FieldAttribute("ProbeUID")>] ProbeUID : int
        /// Feature control type
        [<AgilentControlTypeConverter>]
        [<FieldAttribute("ControlType")>] ControlType : AgilentControlType
        /// 
        [<FieldAttribute("ProbeName")>] ProbeName : string
        /// 
        [<FieldAttribute("PositionX")>] PositionX : float
        /// 
        [<FieldAttribute("PositionY")>] PositionY : float
        /// 
        [<FieldAttribute("gIsFound")>] IsFound_green : bool
        /// 
        [<FieldAttribute("rIsFound")>] IsFound_red : bool
        /// 
        [<FieldAttribute("gProcessedSignal")>] ProcessedSignal_green : float
        /// 
        [<FieldAttribute("rProcessedSignal")>] ProcessedSignal_red : float
        /// 
        [<FieldAttribute("gProcessedSigError")>] ProcessedSigError_green : float
        /// 
        [<FieldAttribute("rProcessedSigError")>] ProcessedSigError_red : float                
        /// Mean raw signal calculated from the intensities of all inlier pixels that represents the feature (after outlier pixel rejection). Also called the foreground signal.
        [<FieldAttribute("gMeanSignal")>] MeanSignal_green : float
        /// 
        [<FieldAttribute("rMeanSignal")>] MeanSignal_red : float
        // Median raw signal calculated from the intensities of all inlier pixels that represents the feature (after outlier pixel rejection). Also called the foreground signal.
        [<FieldAttribute("gMedianSignal")>] MedianSignal_green : float
        /// 
        [<FieldAttribute("rMedianSignal")>] MedianSignal_red : float                
        /// 
        [<FieldAttribute("gBGMeanSignal")>] BGMeanSignal_green : float
        /// 
        [<FieldAttribute("rBGMeanSignal")>] BGMeanSignal_red : float
        /// 
        [<FieldAttribute("gBGMedianSignal")>] BGMedianSignal_green : float
        /// 
        [<FieldAttribute("rBGMedianSignal")>] BGMedianSignal_red : float
        /// 
        [<FieldAttribute("gIsSaturated")>] IsSaturated_green : bool
        /// 
        [<FieldAttribute("rIsSaturated")>] IsSaturated_red : bool
        ///         
        [<FieldAttribute("gBGSubSignal")>] BGSubSignal_green : float
        /// 
        [<FieldAttribute("rBGSubSignal")>] BGSubSignal_red : float
        /// 
        [<FieldAttribute("gBGSubSigError")>] BGSubSigError_green : float
        /// 
        [<FieldAttribute("rBGSubSigError")>] BGSubSigError_red : float        
        /// 
        [<FieldAttribute("gIsWellAboveBG")>] IsWellAboveBG_green : bool
        /// 
        [<FieldAttribute("rIsWellAboveBG")>] IsWellAboveBG_red : bool                
        }   


    /// Reads agilent raw data from file
    let readAgilentDataRaw path =    
        let reader = new CsvReader<AgilentDataRaw>(schemaMode = SchemaMode.Fill)
        reader.ReadFile(path, '\t', true,skipLinesBeforeHeader = 9)

    

    type AgilentRawDescription = { 
        [<FieldAttribute("ProbeName")>]         ProbeName : string
        [<FieldAttribute("GeneName")>] GeneName : string
        [<FieldAttribute("SystematicName")>] SystematicName : string
        [<FieldAttribute("Description")>] Description : string
        //  Start position of the probe sequence
        [<FieldAttribute("Start")>] Start : int
        // The probe sequence as specified in the design file
        [<FieldAttribute("Sequence")>] Sequence : string
        //  Unique integer for each unique probe in a design
        [<FieldAttribute("ProbeUID")>] ProbeUID : int



    }
