namespace FSharpBio



type MassIndexKey 
    ( massKey        : float,
      peptidePointer : int64
    ) = 
    member this.MassKey        = massKey
    member this.PeptidePointer = peptidePointer
        
    override this.ToString() =
        sprintf "MassIndexEntry: MassKey=%f; PeptidePointer=%A" this.MassKey this.PeptidePointer








//
//
//
//
//
//
//
//type MassIndexNode = {
//    Entries : 
//    }
//        internal class MassIndexNode
//        {
//
//            public MassIndexNode(long filePosition, int maxChildren)
//            {
//                FilePosition = filePosition;
//                Entries = new List<MassIndexKey>(maxChildren - 1);
//                Children = new List<long>(maxChildren);
//            }
//
//            public List<MassIndexKey> Entries { get; private set; }
//            public List<long> Children { get; private set; }
//
//            public long FilePosition { get; internal set; }
//
//            public bool IsLeaf
//            {
//                get
//                {
//                    return this.Children.Count == 0;
//                }
//            }
//
//            public int NumberOfChildren
//            {
//                get { return Children.Count; }
//            }
//
//            public int NumberOfEntries
//            {
//                get { return Entries.Count; }
//            }
//
//        }
//
////module PeptideLookUp
//
