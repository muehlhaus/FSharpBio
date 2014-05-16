#if INTERACTIVE
    #light
    #r "System.Xml.dll"
    #r "System.Runtime.Serialization.dll"
    #r "D:/Development/FSharp/FSharpBio/bin/FSharp.CoreX.dll"
    #r "D:/Development/FSharp/FSharpBio/bin/FSharpBio.IO.dll"
    (*[omit:(other references omitted)]*)
    #r "D:/Development/FSharp/FSharpBio/packages/NUnit.2.6.3/lib/nunit.framework.dll"
    #r "D:/Development/FSharp/FsharpBio/packages/FsUnit.1.2.1.0/Lib/Net40/FsUnit.NUnit.dll"
(*[/omit]*)
#else
namespace FSharpBio.IO.Tests

module Serialiaztion = 
#endif

    open FSharpBio.IO.Serialization
    open System.Runtime.Serialization

    open NUnit.Framework
    open FsUnit


    [<DataContract>]
    type TestRecordType = { 
        [< field : DataMember(Name="Id") >]
        Id   : int
        [< field : DataMember(Name="Name") >]
        Name : string
        }




    [<Test>]
    let ``should return the JSON string`` () =
            let dataTestRecordType = { Id = 1; Name = "myName";}

            let jsonString = "{\"Id\":1,\"Name\":\"myName\"}"
            serializeJson<TestRecordType>(dataTestRecordType) |> should equal jsonString

    open Microsoft.FSharp.Reflection
    open System.Reflection


    open Microsoft.FSharp.Reflection  
    open System.Reflection  
    open System.Runtime.Serialization  
    open System.Runtime.Serialization.Json  
    open System.Xml 

    open System

    // IDataContractSurrogate






    [<KnownType("KnownTypes")>]
    type TestUnionType = 
        | U1 of string * int
        | U2 of int
        static member KnownTypes() = 
                typeof<TestUnionType>.GetNestedTypes(
                    BindingFlags.Public 
                    ||| BindingFlags.NonPublic) |> Array.filter FSharpType.IsUnion


    let union = TestUnionType.U1 ("test", 45)
    let _ = serializeJson<TestUnionType>(union)
    let a : TestUnionType = deserializeXml( serializeXml<TestUnionType>(union))


    open Microsoft.FSharp.Reflection


    type Cluster<'T> = 
        | Node of int * float * Cluster<'T> * Cluster<'T>
        | Leaf of int * 'T



    // Test print a tree.
    let rec p (c:Cluster<'T>) = 
        match c with
        | Node (id,distance,lc,rc) -> printfn "{ Name : %i," id
                                      p lc
                                      p rc
        | Leaf (id,_)              -> printfn "{ Name : %i }" id


    //let toString (x:'a) = 
    //    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    //    | case, o -> match o with
    //                 | [|id;dist;_;_|] ->  


    //let fromString (t:System.Type) (s:string) =
    //    match FSharpType.GetUnionCases t |> Array.filter (fun case -> case.Name = s) with
    //    |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]))
    //    |_ -> None
    //
    //toString union

