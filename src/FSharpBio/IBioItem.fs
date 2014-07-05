namespace FSharpBio

///Marker interface for BioItem base.
type IBioItem =    
    abstract member Name     : string
    abstract member Symbol   : char
    abstract member ByteCode : byte 
    abstract member Formula  : Formula.Formula
    abstract member isTerminator : bool     
    abstract member isGap        : bool


/// Type abbreviation for converting char to Bioitem
type bioItemConverter<'a when 'a :> IBioItem> = char -> 'a option


/// Type abbreviation for converting char to optional Bioitem
type bioItemOptionConverter<'a when 'a :> IBioItem> = char -> 'a option

