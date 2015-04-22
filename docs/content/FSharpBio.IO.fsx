(*** hide ***)
#I "../../bin"

(**




**)

// ##################################################################
// Examples for Fsharp.FsIO Project
//
// --- Csv-reader
// --- FatsA reader/ writer
#r "FSharp.CoreX.dll"
#r "FSharpBio.dll"

open FSharpBio.IO
open FSharpBio.IO.SchemaReader
open FSharpBio.IO.SchemaReader.Csv
open FSharpBio.IO.SchemaReader.Attribute

// ##################################################################
// Examples: Csv-reader reads iris data set
type irisItem = 
    { [<FieldAttribute("Sepal length")>] SepalLength : float
      [<FieldAttribute("Sepal width")>] SepalWidth : float
      [<FieldAttribute("Petal length")>] PetalLength : float
      [<FieldAttribute("Petal width")>] PetalWidth : float
      [<FieldAttribute("Species")>] Species : string
      [<FieldAttribute("Species2")>] Species2 : string }

//0 based index mapping 
type irisItemWithIndex = 
    { [<FieldAttribute(0)>] SepalLength : float
      [<FieldAttribute(1)>] SepalWidth : float
      [<FieldAttribute(2)>] PetalLength : float
      [<FieldAttribute(3)>] PetalWidth : float
      [<FieldAttribute(4)>] Species : string }

type DoubleArrayConverter() = 
    inherit ConverterAttribute()
    override this.convertToObj = 
        Converter.Collection(fun (strs : seq<string>) -> 
            (strs
             |> Seq.map 
                    (fun s -> FSharp.CoreX.String.tryParseFloatDefault nan s)
             |> Seq.toArray)
            |> box)

type irisItemWithMulti = 
    { [<FieldAttribute([| 0; 1; 2; 3 |])>][<DoubleArrayConverter>] Features : float []
      [<FieldAttribute(4)>] Species : string }

//let _         = IO.setWorkingDirectory __SOURCE_DIRECTORY__
let path = "./Examples/Data/irisData.csv" //....
let reader = new CsvReader<irisItem>(schemaMode = SchemaMode.Fill)
let hasHeader = true
let separator = ','
let data = reader.ReadFile(path, separator, hasHeader)






// ##################################################################
// Examples: Reade fastA file
// Converter reads character and returns it as character
// for protein fasta use:  AminoAcids.oneLetterCodeAsObject  (from FsBIO)
// for gene fasta use:     Nucleotides.oneLetterCodeAsObject (from FsBIO)
let converter (letter : System.Char) = letter
//let _         = IO.setWorkingDirectory __SOURCE_DIRECTORY__
let fastaPath = "./Examples/Data/chlamy3proteins.fasta"
// Read .fasta
let chlamy3proteins = FastA.fromFile (converter) fastaPath |> Seq.toArray
// Write
let _ = chlamy3proteins |> FastA.write converter fastaPath
