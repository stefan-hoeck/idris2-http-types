module HTTP.Parser.Header

import Data.Buffer
import Data.Either
import Data.SortedMap
import Derive.Prelude
import HTTP.Header.Types
import HTTP.Parser.Util
import Syntax.T1
import Text.ILex.DStack

%default total
%hide Data.Linear.(.)
%language ElabReflection

mtype : ByteString -> (String,String)
mtype bs = let (x,y) := break (47 ==) bs in (toString x, toString $ drop 1 y)

md : ByteString -> MediaDesc
md bs = let (x,y) := mtype bs in MD x y

mt : ByteString -> MediaType
mt bs = let (x,y) := mtype bs in MT x y

mdstar : ByteString -> MediaDesc
mdstar = MDStar . toString . dropEnd 2

--------------------------------------------------------------------------------
-- Regular Expressions
-- (see Appendix A from [RFC 91110](https://www.rfc-editor.org/rfc/rfc9110.txt)
--------------------------------------------------------------------------------

qdtext : RExp True
qdtext =
  plus $ HTAB <|> SP <|> '!' <|> range32 0x23 0x5b <|> range32 0x5d 0x7e

quotedPair : RExp True
quotedPair = '\\' >> (HTAB <|> SP <|> Ch VCHAR)

field : RExp True
field = plus (HTAB <|> SP <|> Ch VCHAR) >> CRLF

public export
dayname : RExp True
dayname = "Mon" <|> "Tue" <|> "Wed" <|> "Thu" <|> "Fri" <|> "Sat" <|> "Sun"

export
daynameL : RExp True
daynameL =
     "Monday"
 <|> "Tuesday"
 <|> "Wednesday"
 <|> "Thursday"
 <|> "Friday"
 <|> "Saturday"
 <|> "Sunday"

public export
second : RExp True
second = digit >> digit

public export
minute : RExp True
minute = digit >> digit

public export
hour : RExp True
hour = digit >> digit

public export
timeOfDay : RExp True
timeOfDay = hour >> ':' >> minute >> ':' >> second

public export
month : RExp True
month =
      "Jan" <|> "Feb" <|> "Mar" <|> "Apr" <|> "May" <|> "Jun"
  <|> "Jul" <|> "Aug" <|> "Sep" <|> "Oct" <|> "Nov" <|> "Dec"

public export
year : RExp True
year = repeat 4 digit

export
qvalue : RExp True
qvalue =
      ('0' >> opt ('.' >> atmost 3 digit))
  <|> ('1' >> opt ('.' >> atmost 3 '0'))

token : RExp True
token = plus (alphaNum <|> Ch tokenChar)

--------------------------------------------------------------------------------
-- Headers Parser
--------------------------------------------------------------------------------

public export
data HState : List Type -> Type where
  HMap    : HState [HeaderMap]
  HNam    : HState [String]
  HPar    : HState [SnocList Parameter]
  HParS   : HState [Void]
  HParN   : HState [String,SnocList Parameter]
  HParQ   : HState [Void]
  HVal1   : HState []
  HVal    : HState [String]
  HAcc    : HState [SnocList MediaRange]
  HAccD   : HState [MediaDesc,SnocList MediaRange]
  HField  : HState [Void]
  HStr    : HState [Void]
  HNat    : HState [Nat]
  HMT     : HState []
  HMT1    : HState [MediaType]
  HCD     : HState []
  HCD1    : HState [String]
  HEnd    : HState [Void]
  HErr    : HState []

%runElab deriveIndexed "HState" [Show,ConIndex]

public export
data HRes : HState ts -> Stack False HState ts -> Type -> Type where
  RMap  : HRes HMap  [SortedMap.empty] HeaderMap
  RAcc  : HRes HAcc  [[<]]  (List MediaRange)
  RConL : HRes HNat  [0]    Nat
  RConT : HRes HMT   []     ContentType
  RConD : HRes HCD   []     ContentDisp
  RVal  : HRes HVal1 []     String

HSz : Bits32
HSz = 1 + cast (conIndexHState HErr)

inBoundsHState : (s : HState ts) -> (cast (conIndexHState s) < HSz) === True

export %inline
Cast (HState ts) (Index HSz) where
  cast v = I (cast $ conIndexHState v) @{mkLT $ inBoundsHState v}

public export
0 SK : Type -> Type
SK = DStack HState Void

parameters {auto sk : SK q}
  hfield : ByteString -> StateAct q HState HSz
  hfield b HNam (n::HMap:>[m]) = dput HMap [insert n b m]
  hfield _ st   x              = derr HErr st x

  meddesc : MediaDesc -> StateAct q HState HSz
  meddesc md HAcc x t = dput HPar ([<]::HAccD:>md::x) t
  meddesc md st   x t = derr HErr st x t

  medtype : MediaType -> StateAct q HState HSz
  medtype m HMT x t = dput HPar ([<]::HMT1:>[m]) t
  medtype m st  x t = derr HErr st x t

  condisp : ByteString -> StateAct q HState HSz
  condisp bs HCD x t = dput HPar ([<]::HCD1:>[toString bs]) t
  condisp _  st  x t = derr HErr st x t

  hendpar : StateAct q HState HSz
  hendpar HPar (sp::HAccD:>md::sd::x) = dput HAcc $ (sd:<MR md (sp<>>[]))::x
  hendpar st   x                      = derr HErr st x

  pname : String -> StateAct q HState HSz
  pname s HPar x = dput HParN (s::x)
  pname s st   x = derr HErr st x

  pvalue : String -> StateAct q HState HSz
  pvalue v HParN (n::sp::x) = dput HPar $ (sp:<P n v)::x
  pvalue v HVal1 x          = dput HVal [v]
  pvalue v st    x          = derr HErr st x

  qval : Double -> StateAct q HState HSz
  qval v HPar (sp::x) = dput HPar $ (sp:<Q v)::x
  qval v st   x       = derr HErr st x

  hstr : String -> StateAct q HState HSz
  hstr s st x = pvalue s st x

spaced : HState ts -> Steps q HSz SK -> DFA q HSz SK
spaced r ss = dfa $ [conv' (plus WSP) r] ++ ss

headerTrans : Lex1 q HSz SK
headerTrans =
  lex1
    [ entry HMap $ dfa [read token $ dpush HNam . toUpper, newline' CRLF HEnd]
    , entry HNam $ dfa [cexpr' ':' HField]
    , entry HAcc $ spaced HAcc
        [ cexpr' ',' HAcc
        , cexpr "*/*" $ dact (meddesc MDAny)
        , conv (token >> "/*") $ dact . meddesc . mdstar
        , conv (token >> "/" >> token) $ dact . meddesc . md
        ]
    , entry HPar $ spaced HPar [cexpr' ';' HParS, cexpr ',' $ dact hendpar]
    , entry HParS $ spaced HParS
        [ cexpr' ';' HParS
        , cexpr ',' $ dact hendpar
        , cexpr' (like "q=") HParQ
        , read token $ dact . pname
        ]
    , entry HParN $ dfa [cexpr' '=' HVal1]
    , entry HParQ $ dfa
        [ cexpr "1." $ dact $ qval 1.0
        , cexpr "0." $ dact $ qval 0.0
        , conv qvalue $ dact . qval . cast . toString
        ]
    , entry HVal1 $ dfa [read token $ dact . pvalue, copen' '"' HStr]
    , entry HNat $ dfa [conv (plus digit) $ \bs => dput HNat [cast $ integer bs]]
    , entry HMT $ dfa [conv (token >> "/" >> token) $ dact . medtype . mt]
    , entry HCD $ dfa [conv token $ dact . condisp]
    , entry HStr $ dfa
        [ ccloseStr '"' $ dact . hstr
        , read qdtext $ pushStr HStr
        , conv quotedPair $ pushStr HStr . toString . drop 1
        ]
    , entry HField $ spaced HField [convline field $ dact . hfield . trimRight]
    ]

headerErr : Arr32 HSz (SK q -> F1 q (BoundedErr Void))
headerErr = errs []

end : HRes st x t -> HState ts -> Stack b HState ts -> Maybe t
end RMap  HMap   [m]                 = Just m
end RAcc  HAcc   [sm]                = Just $ sm<>>[]
end RAcc  HAccD  [d,sm]              = Just $ sm<>>[MR d []]
end RVal  HVal   [s]                 = Just s
end RConL HNat   [n]                 = Just n
end RConT HMT1   [m]                 = Just $ CT m []
end RConD HCD1   [v]                 = Just $ CD v []
end RAcc  HPar   (sp::HAccD:>[d,sm]) = Just $ sm<>>[MR d $ sp<>>[]]
end RConT HPar   (sp::HMT1:>[m])     = Just $ CT m (sp <>>[])
end RConD HPar   (sp::HCD1:>[s])     = Just $ CD s (sp <>>[])
end _     _      _                   = Nothing

headerEOI : HRes st x v -> Index HSz -> SK q -> F1 q (Either (BoundedErr Void) v)
headerEOI res sk s t =
  let (st:>x) # t := read1 s.stack_ t
      Nothing     := end res st x | Just v => Right v # t
   in arrFail SK headerErr sk s t

export
header : {st : _} -> {x : _} -> HRes st x t -> P1 q (BoundedErr Void) HSz SK t
header res =
  P (cast st) (init $ st:>x) headerTrans (\x => (Nothing #))
    headerErr (headerEOI res)

export
headerMay : {st : _} -> {x : _} -> HRes st x t -> ByteString -> Maybe t
headerMay res bs = eitherToMaybe $ parseBytes (header res) Virtual bs

export
testHeader : {st : _} -> {x : _} -> HRes st x v -> Show v => String -> IO ()
testHeader res s =
  case parseString (header res) Virtual s of
    Left x => putStrLn "\{x}"
    Right res => printLn res

--------------------------------------------------------------------------------
-- Proofs
--------------------------------------------------------------------------------

inBoundsHState HMap    = Refl
inBoundsHState HNam    = Refl
inBoundsHState HPar    = Refl
inBoundsHState HParQ   = Refl
inBoundsHState HParS   = Refl
inBoundsHState HParN   = Refl
inBoundsHState HVal    = Refl
inBoundsHState HVal1   = Refl
inBoundsHState HAcc    = Refl
inBoundsHState HAccD   = Refl
inBoundsHState HField  = Refl
inBoundsHState HStr    = Refl
inBoundsHState HEnd    = Refl
inBoundsHState HNat    = Refl
inBoundsHState HMT     = Refl
inBoundsHState HMT1    = Refl
inBoundsHState HCD     = Refl
inBoundsHState HCD1    = Refl
inBoundsHState HErr    = Refl
