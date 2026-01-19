module HTTP.Header.Types

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
  type   : String
  subtye : String
  params : Parameters

%runElab derive "MediaType" [Show,Eq]

public export
0 MediaRanges : Type
MediaRanges = List MediaRange

public export
data HeaderVal : Type where
  Accept        : MediaRanges -> HeaderVal
  ContentLength : Nat -> HeaderVal
  ContentType   : MediaType -> HeaderVal
  Other         : String -> HeaderVal

%runElab derive "HeaderVal" [Show,Eq]

public export
0 HeaderMap : Type
HeaderMap = SortedMap String HeaderVal
