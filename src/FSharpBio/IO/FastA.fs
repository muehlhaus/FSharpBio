namespace FSharpBio.IO

open System
open FSharp.CoreX

    
module FastA =
            
    /// Fasta item contains header and sequence
    type FastaItem<'a> = {
        Header    : string;
        Sequence  : 'a;       
    }


    /// <summary>Creates <c>FastaItem</c> with header and sequence.
    /// </summary>
    /// <param name="header">The header line of a fasta entry</param>
    /// <param name="sequence">The sequence</param>
    /// <returns>A new <c>FastaItem</c>.</returns>        
    let createFastaItem header sequence =
        { Header = header; Sequence = sequence }

        
    // Conditon of grouping lines
    let private same_group l =             
        not (String.length l = 0 || l.[0] <> '>')
    
    // Matches grouped lines and concatenates them
    let private record d (converter:char -> 'a) = 
        match d with
        | [] -> raise (System.Exception "Incorrect FASTA format")
        | (h:string) :: t when h.StartsWith ">" ->  let header = h .Remove(0,1)
                                                    let sequence = Seq.concat t |> Seq.map converter
                                                    createFastaItem header sequence
                                                        
        | h :: _ -> raise (System.Exception "Incorrect FASTA format")

                
    /// <summary>Reads <c>FastaItem</c> from file.
    /// </summary>
    /// <param name="converter">Determines type of sequence by converting <c>char -> type</c> </param>
    /// <param name="filePath">File path</param>
    /// <returns>A new seq of <c>FastaItem</c>.</returns>         
    let fromFile converter (filePath) =
        FileIO.readFile filePath
        |> Seq.filter (fun (l:string) -> not (l.StartsWith ";" || l.StartsWith "#"))
        |> Seq.groupWhen same_group 
        |> Seq.map (fun l -> record (List.ofSeq l) converter)

                  
    /// <summary>Writes <c>FastaItem</c> to file.
    /// </summary>
    /// <param name="toString">Determines type of sequence by converting <c>type -> char</c> </param>
    /// <param name="filePath">File path</param>
    /// <param name="data">Seq of <c>FastaItems</c></param>
    /// <returns>unit</returns>         
    let write (toString:'T -> char) (filePath:string) (data:seq<FastaItem<seq<'T>>>) =
        let toChunks (w:System.IO.StreamWriter) (length:int) (source: seq<'T>) =    
            use ie = source.GetEnumerator()
            let sourceIsEmpty = ref false
            let builder = System.Text.StringBuilder(length)
            let rec loop () =        
                    if ie.MoveNext () then                
                        builder.Append(toString ie.Current) |> ignore
                        for x in 2 .. length do
                            if ie.MoveNext() then
                                builder.Append(toString ie.Current) |> ignore
                            else
                                sourceIsEmpty := true                
                
                        match !sourceIsEmpty with
                        | false -> // writer builder
                                   w.WriteLine(builder.ToString())
                                   builder.Clear() |> ignore
                                   loop ()
                        | true  -> w.WriteLine(builder.ToString())
                                   ()
        
            loop ()
        use sWriter = new System.IO.StreamWriter(filePath)
        data
        |> Seq.iter (fun (i:FastaItem<_>) ->
                                sWriter.WriteLine(">" + i.Header)
                                toChunks sWriter 80 i.Sequence)        





