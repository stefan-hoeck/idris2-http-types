module HTTP.Header.Types

import Data.ByteString
import Data.SortedMap
import Derive.Prelude

%default total
%language ElabReflection

public export
data Parameter : Type where
  P : (name, value : String) -> Parameter
  Q : Double -> Parameter

%runElab derive "Parameter" [Show,Eq]

public export
0 Parameters : Type
Parameters = List Parameter

public export
data MediaDesc : Type where
  MDAny  : MediaDesc
  MDStar : (type : String) -> MediaDesc
  MD     : (type, subtype : String) -> MediaDesc

%runElab derive "MediaDesc" [Show,Eq]

public export
record MediaRange where
  constructor MR
  type   : MediaDesc
  params : Parameters

%runElab derive "MediaRange" [Show,Eq]

public export
record MediaType where
  constructor MT
  type    : String
  subtype : String

export
encodeMediaType : MediaType -> ByteString
encodeMediaType (MT t s) = fromString "\{t}/\{s}"

%runElab derive "MediaType" [Show,Eq]

public export
record ContentType where
  constructor CT
  type   : MediaType
  params : Parameters

%runElab derive "ContentType" [Show,Eq]

export
accepts : MediaDesc -> MediaType -> Bool
accepts MDAny             _ = True
accepts (MDStar type)     t = type == t.type
accepts (MD type subtype) t = type == t.type && subtype == t.subtype

public export
0 MediaRanges : Type
MediaRanges = List MediaRange

public export
0 HeaderMap : Type
HeaderMap = SortedMap String ByteString
