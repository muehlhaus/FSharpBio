namespace FSharpBio.Mz

module PMI =

    open System
    open System.IO
    open System.Linq
    open System.Collections.Generic
    open Microsoft.FSharp.Core.Operators.Unchecked

    type MassIndexKey(mass:double, pointer:int64) = class
        member this.MassKey with get() = mass
        member this.PeptidePointer with get() = pointer

        override this.ToString() =
            String.Format("MassIndexEntry: MassKey={0}; PeptidePointer={1}", this.MassKey, this.PeptidePointer)
    end

    type MassIndexKeyComparer() = 
        interface IComparer<MassIndexKey> with 
            member this.Compare(x:MassIndexKey, y:MassIndexKey) =
                x.MassKey.CompareTo(y.MassKey)
    
    type MassIndexNode(filePosition:int64, maxChildren:int) = class
        let entries = new List<MassIndexKey>(maxChildren - 1)
        let children = new List<int64>()
        member this.Entries with get() = entries
        member this.Children with get() = children
        member this.FilePosition with get() = filePosition
        member this.IsLeaf with get() = children.Count = 0
        member this.NumberOfChildren with get() = children.Count
        member this.NumberOfEntries with get() = entries.Count
    end

    type MassIndexNodeIO(stream:Stream, maxChildrenPerNode:int) = class

        let mutable stream:Stream = stream
        let maxEntries = maxChildrenPerNode - 1
        let maxChildren = maxChildrenPerNode
        let blockSize = sizeof<int> + // entry count
                        ((sizeof<double> + sizeof<int64>) * maxEntries) +  // entries     
                        sizeof<int> + // child count
                        (sizeof<int64> * maxChildren); // child pointer
        let mutable cacheSize = 50000
        let mutable nodeCache:Dictionary<int64, MassIndexNode> = new Dictionary<int64, MassIndexNode>(cacheSize)
        let readWriteBuffer:byte[] = Array.zeroCreate blockSize
        let allocBuffer:byte[] = Array.zeroCreate blockSize
        let reader = new BinaryReader(new MemoryStream(readWriteBuffer))
        let writer= new BinaryWriter(new MemoryStream(readWriteBuffer))
        let mutable isCachingEnabled = true

        member this.IsCachingEnabled with get() = isCachingEnabled
                                     and set(value) = if (isCachingEnabled && value = false) then                
                                                        this.ClearCache()                
                                                        isCachingEnabled <- value

        member this.CacheSize with get() = cacheSize
                              and set(value) = if (cacheSize <> value) then                
                                                    this.ClearCache()
                                                    cacheSize <- value
                                                    nodeCache <- new Dictionary<int64, MassIndexNode>(cacheSize);

        member val NumberOfIOReads = 0 with get, set
        member val NumberOfIOWrites = 0 with get, set
                
        member internal this.ClearCache() =        
            for n in nodeCache.Values do
                this.DiscWriteInternal(n)
            nodeCache.Clear()  

        member internal this.Close() = 
            if (stream <> null) then            
                this.ClearCache()
                stream.Flush()
                stream.Close()
                stream <- null           

        member internal this.CachePrune() =        
            if (nodeCache.Count > cacheSize) then         
                let key = nodeCache.Keys.First()
                this.DiscWriteInternal(nodeCache.[key])
                nodeCache.Remove(key) |> ignore         

        member internal this.WriteDegree(degree:int) =     
            let writer = new BinaryWriter(stream);
            writer.BaseStream.Seek(0L, SeekOrigin.Begin) |> ignore
            writer.Write(degree);        

        static member internal ReadDegree(stream:Stream) =
            let reader = new BinaryReader(stream)
            reader.BaseStream.Seek(0L, SeekOrigin.Begin) |> ignore
            reader.ReadInt32()
        
        member internal this.WriteRootPointer(pointer:int64) =
            let writer = new BinaryWriter(stream)
            writer.BaseStream.Seek(sizeof<int> |> int64, SeekOrigin.Begin) |> ignore
            writer.Write(pointer);
        
        member internal this.ReadRootPointer() =     
            let reader = new BinaryReader(stream)
            reader.BaseStream.Seek(sizeof<int> |> int64, SeekOrigin.Begin)  |> ignore
            reader.ReadInt64()       

        member internal this.ReadRoot() = this.DiskRead(this.ReadRootPointer())        

        member private this.Seek(fp:int64)  =      
            if (fp <> stream.Position) then            
                stream.Seek(fp, SeekOrigin.Begin) |> ignore

        member private this.DiskReadInternal(filePointer:int64) =
            let n = new MassIndexNode(filePointer, maxChildren)
            this.Seek(n.FilePosition)
            stream.Read(readWriteBuffer, 0, readWriteBuffer.Length) |> ignore
            reader.BaseStream.Seek(0L, SeekOrigin.Begin) |> ignore
            let entryCount = reader.ReadInt32()
            let childCount = reader.ReadInt32()

            if (childCount = 0) then            
                for i in 0 .. entryCount-1 do                
                    reader.BaseStream.Seek(sizeof<int64> |> int64, SeekOrigin.Current) |> ignore
                    let mass = reader.ReadDouble()
                    let pointer = reader.ReadInt64()
                    let key = new MassIndexKey(mass, pointer)
                    n.Entries.Add(key)             
            else            
                for i in 0 .. entryCount-1 do                 
                    n.Children.Add(reader.ReadInt64())
                    let mass = reader.ReadDouble()
                    let pointer = reader.ReadInt64()
                    let key = new MassIndexKey(mass, pointer)
                    n.Entries.Add(key)
                
                n.Children.Add(reader.ReadInt64())
            
            this.NumberOfIOReads <- this.NumberOfIOReads + 1
            n

        member internal this.DiskRead(filePointer:int64) =
        
            let mutable n = defaultof<MassIndexNode>

            if this.IsCachingEnabled = false then
                n <- this.DiskReadInternal(filePointer)            
            else if nodeCache.ContainsKey(filePointer) = false then            
                //CachePrune();
                n <- this.DiskReadInternal(filePointer);
                //nodeCache.Add(n.FilePosition, n); 
            else
                n <- nodeCache.[filePointer]                           
            n
        
        member internal this.DiscAlloc() =
            let n = new MassIndexNode(stream.Length, maxChildren);
            this.Seek(n.FilePosition);
            stream.Write(allocBuffer, 0, allocBuffer.Length);

            if (this.IsCachingEnabled) then            
                this.CachePrune()
                nodeCache.Add(n.FilePosition, n)
            n        

        member internal this.DiscWrite(n:MassIndexNode) =        
            if (this.IsCachingEnabled = false) then            
                this.DiscWriteInternal(n)            
            else if (nodeCache.ContainsKey(n.FilePosition) = false) then            
                this.CachePrune();
                nodeCache.Add(n.FilePosition, n);            

        member internal this.DiscWriteInternal(n:MassIndexNode) = 

            writer.BaseStream.Seek(0L, SeekOrigin.Begin) |> ignore

            let nEntries = n.NumberOfEntries
            let nChilds = n.NumberOfChildren

            writer.Write(nEntries)
            writer.Write(nChilds)

            if (n.IsLeaf) then            
                for i in 0 .. nEntries-1 do                
                    writer.BaseStream.Seek(sizeof<int64> |> int64, SeekOrigin.Current) |> ignore
                    writer.Write(n.Entries.[i].MassKey)
                    writer.Write(n.Entries.[i].PeptidePointer)        
            else            
                for i in 0 .. nEntries-1 do 
                    writer.Write(n.Children.[i])
                    writer.Write(n.Entries.[i].MassKey)
                    writer.Write(n.Entries.[i].PeptidePointer)
                                    
                writer.Write(n.Children.[nEntries])           

            this.Seek(n.FilePosition);

            stream.Write(readWriteBuffer, 0, readWriteBuffer.Length);

            this.NumberOfIOWrites <- this.NumberOfIOWrites + 1

    end

    [<AllowNullLiteral>]
    type PeptideMassIndex() = class
        
        let keyComparer:MassIndexKeyComparer = new MassIndexKeyComparer()

        static member CalcMaxChildren(minDegree:int) = 2 * minDegree

        member private this.HasReachedMaxEntries(n:MassIndexNode) =
            n.NumberOfEntries = ((2 * this.MinDegree) - 1)      

        member private this.HasReachedMinEntries(n:MassIndexNode) =
            n.NumberOfEntries = (this.MinDegree - 1)        

        member val NodeIO = defaultof<MassIndexNodeIO> with get, set 
        member val MinDegree = 0 with get, set
        member val Root = defaultof<MassIndexNode> with get, set

        member this.Shutdown() =        
            this.NodeIO.Close() |> ignore
            this.Root <- defaultof<MassIndexNode>
            this.MinDegree <- 0      

        // *************** index factory *********************************************

        static member Create(idxFileName:string, minDegree:int) =
            if (minDegree < 2) then         
                raise( ArgumentException("BTree degree must be at least 2", "minDegree"))       

            if (File.Exists(idxFileName) = false) then            
                let stream = new FileStream(idxFileName, FileMode.CreateNew, FileAccess.ReadWrite, FileShare.Read)
                let pmi = new PeptideMassIndex()
                pmi.NodeIO <- new MassIndexNodeIO(stream, PeptideMassIndex.CalcMaxChildren(minDegree))
                pmi.MinDegree <- minDegree
                pmi.NodeIO.WriteDegree(minDegree);
                pmi.NodeIO.WriteRootPointer(-1L) |> ignore // alloc space for root file pointer in file after degree value
                pmi.Root <- pmi.NodeIO.DiscAlloc()
                pmi.NodeIO.WriteRootPointer(pmi.Root.FilePosition)
                pmi            
            else            
                raise( IOException("Could not create index file already exists."))
            
        static member Open(idxFileName:string) =
        
            if (File.Exists(idxFileName)) then
                let stream = new FileStream(idxFileName, FileMode.Open, FileAccess.ReadWrite, FileShare.Read)

                if (stream.Length = 0L) then                
                    raise( IOException("Could not open index, index file is empty."))                
                else
                    let minDegree = MassIndexNodeIO.ReadDegree(stream);
                    let nodeIO = new MassIndexNodeIO(stream, PeptideMassIndex.CalcMaxChildren(minDegree))
                    let pmi = new PeptideMassIndex();

                    pmi.NodeIO <- nodeIO;
                    pmi.MinDegree <- minDegree
                    pmi.Root <- nodeIO.ReadRoot()
                    pmi;                
            else
                raise(IOException("Could not open index, index file not found."))

    // *************** index diagnostics *********************************************

        member this.DeepDump() =
            this.DeepDumpInternal(this.Root)
        
        member private this.DeepDumpInternal(node:MassIndexNode) =
        
            if (node.IsLeaf) then
                for i in 0 .. node.NumberOfEntries-1 do   
                    Console.Out.WriteLine(String.Format("{0} [leaf #e={1}]", node.Entries.[i], node.NumberOfEntries))            

            for i in 0 .. node.NumberOfEntries-1 do            
                if (i > 0) then                
                    Console.Out.WriteLine(String.Format("{0} [inner #e={1} #c={2}]", node.Entries.[i - 1], node.NumberOfEntries, node.NumberOfChildren))                
                this.DeepDumpInternal(this.NodeIO.DiskRead(node.Children.[i]))           

        member this.GetCurrentDepth() =
            this.GetCurrentDepthInternal(this.Root, 1)

        member private this.GetCurrentDepthInternal(node:MassIndexNode, depth:int) =        
            if (node.IsLeaf) then
                depth         
            else
                this.GetCurrentDepthInternal(this.NodeIO.DiskRead(node.Children.[0]), depth + 1)        
        

    // *************** index insert *********************************************
           
            
        /// <summary>
        /// Helper method that splits a full node into two nodes.
        /// </summary>
        /// <param name="parentNode">Parent node that contains node to be split.</param>
        /// <param name="nodeToBeSplitIndex">Index of the node to be split within parent.</param>
        /// <param name="nodeToBeSplit">Node to be split.</param>
        member private this.SplitChild(parentNode:MassIndexNode, nodeToBeSplitIndex:int, nodeToBeSplit:MassIndexNode) =
        
            let newNode = this.NodeIO.DiscAlloc()

            parentNode.Entries.Insert(nodeToBeSplitIndex, nodeToBeSplit.Entries.[this.MinDegree - 1])
            parentNode.Children.Insert(nodeToBeSplitIndex + 1, newNode.FilePosition)

            newNode.Entries.AddRange(nodeToBeSplit.Entries.GetRange(this.MinDegree, this.MinDegree - 1))

            // remove also Entries[this.Degree - 1], which is the one to move up to the parent
            nodeToBeSplit.Entries.RemoveRange(this.MinDegree - 1, this.MinDegree)

            if (nodeToBeSplit.IsLeaf = false) then            
                newNode.Children.AddRange(nodeToBeSplit.Children.GetRange(this.MinDegree, this.MinDegree))
                nodeToBeSplit.Children.RemoveRange(this.MinDegree, this.MinDegree)         

            this.NodeIO.DiscWrite(nodeToBeSplit)
            this.NodeIO.DiscWrite(newNode)
            this.NodeIO.DiscWrite(parentNode)
            newNode

        member private this.InsertNonFull(node:MassIndexNode,  newKey:double, newPointer:int64) =
            let newEntry = new MassIndexKey(newKey, newPointer)
            let mutable positionToInsert = 0
            let pos = node.Entries.BinarySearch(newEntry, keyComparer)

            if (pos < 0) then
                positionToInsert <- ~~~pos
            else
                positionToInsert <- pos

            //int positionToInsert = node.Entries.TakeWhile(entry => newKey.CompareTo(entry.MassKey) >= 0).Count();

            // leaf node
            if (node.IsLeaf) then            
                node.Entries.Insert(positionToInsert, newEntry)
                this.NodeIO.DiscWrite(node)
            else
                // non-leaf
                let mutable child = this.NodeIO.DiskRead(node.Children.[positionToInsert])
                if (this.HasReachedMaxEntries(child)) then                
                    let tmp = this.SplitChild(node, positionToInsert, child)
                    if (newKey > node.Entries.[positionToInsert].MassKey) then                    
                        //positionToInsert++;
                        //child = NodeIO.DiskRead(node.Children[positionToInsert]);
                        child <- tmp                                  

                this.InsertNonFull(child, newKey, newPointer);        
     
        /// <summary>
        /// Inserts a new key associated with a pointer in the BTree. This
        /// operation splits nodes as required to keep the BTree properties.
        /// </summary>
        /// <param name="newKey">Key to be inserted.</param>
        /// <param name="newPointer">Pointer to be associated with inserted key.</param>
        member this.Insert(newKey:double, newPointer:int64) =
                // there is space in the root node
                if (this.HasReachedMaxEntries(this.Root) = false) then                
                    this.InsertNonFull(this.Root, newKey, newPointer)
                else                
                    // need to create new node and have it split
                    let oldRoot = this.Root;
                    this.Root <- this.NodeIO.DiscAlloc();
                    this.NodeIO.WriteRootPointer(this.Root.FilePosition);
                    this.Root.Children.Add(oldRoot.FilePosition);
                    this.NodeIO.DiscWrite(this.Root);

                    // split and insert
                    this.SplitChild(this.Root, 0, oldRoot) |> ignore
                    this.InsertNonFull(this.Root, newKey, newPointer);  
                     

    // *************** index search *********************************************
            
        /// <summary>
        /// Helper method that search for a key in a given BTree.
        /// </summary>
        /// <param name="node">Node used to start the search.</param>
        /// <param name="key">Key to be searched.</param>
        /// <returns>Entry object with key information if found, null otherwise.</returns>
        member private this.SearchInternal(node:MassIndexNode, key:double) =        
            let mutable i = 0

            while (i < node.NumberOfEntries && key > node.Entries.[i].MassKey) do            
                i <- i + 1            

            if (i < node.NumberOfEntries && node.Entries.[i].MassKey = key) then
                node.Entries.[i] 
            else if node.IsLeaf then
                defaultof<MassIndexKey>
            else
                this.SearchInternal(this.NodeIO.DiskRead(node.Children.[i]), key);
        
        /// <summary>
        /// Searches a key in the BTree, returning the entry with it and with the pointer.
        /// </summary>
        /// <param name="key">Key being searched.</param>
        /// <returns>Entry for that key, null otherwise.</returns>
        member this.Search(key:double) = this.SearchInternal(this.Root, key)
        
        /// <summary>
        /// Helper method that search for a key range in a given BTree.
        /// </summary>
        /// <param name="node">Node used to start the search.</param>
        /// <param name="key">Key to be searched.</param>
        /// <param name="results">A list to collect the results.</param>
        member private this.SearchInternal(results:List<MassIndexKey> , node:MassIndexNode, low:double, high:double) =

            let mutable i = 0
            while (i < node.NumberOfEntries && low > node.Entries.[i].MassKey) do
                i<-i+1

            let mutable k = i

            for x in i .. node.NumberOfEntries-1 do            
                let e = node.Entries.[x]                
                //Console.Out.WriteLine(e);
                if (e.MassKey >= low && e.MassKey <= high) then                
                    results.Add(e);  
                    k <- x              
//                else                
//                    break                
            
            if (node.IsLeaf = false) then                
                for j in i .. k do                
                    this.SearchInternal(results, this.NodeIO.DiskRead(node.Children.[j]), low, high);
             
             
        /// <summary>
        /// Searches a key range in the BTree, returning the entries with it and with the pointer.
        /// </summary>
        /// <param name="low">lower bound of key range being searched.</param>
        /// <param name="high">higher bound of key range being searched.</param>
        /// <returns>List of entries having key in given range.</returns>
        member this.Search(low:double, high:double) =        
            let results = new List<MassIndexKey>()
            this.SearchInternal(results, this.Root, low, high)
            results           
        
    end


