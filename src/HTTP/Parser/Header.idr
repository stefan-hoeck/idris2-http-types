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
data HState : SnocList Type -> Type where
  HMap    : HState [<HeaderMap]
  HNam    : HState [<String]
  HPar    : HState [<SnocList Parameter]
  HParS   : HState [<Void]
  HParN   : HState [<SnocList Parameter,String]
  HParQ   : HState [<Void]
  HVal1   : HState [<]
  HVal    : HState [<String]
  HAcc    : HState [<SnocList MediaRange]
  HAccD   : HState [<SnocList MediaRange,MediaDesc]
  HField  : HState [<Void]
  HStr    : HState [<Void]
  HNat    : HState [<Nat]
  HMT     : HState [<]
  HMT1    : HState [<MediaType]
  HCD     : HState [<]
  HCD1    : HState [<String]
  HEnd    : HState [<Void]
  HErr    : HState [<]

%runElab deriveIndexed "HState" [Show,ConIndex]

public export
data HRes : HState st -> Stack False HState st -> Type -> Type where
  RMap  : HRes HMap  [<SortedMap.empty] HeaderMap
  RAcc  : HRes HAcc  [<[<]]  (List MediaRange)
  RConL : HRes HNat  [<0]    Nat
  RConT : HRes HMT   [<]     ContentType
  RConD : HRes HCD   [<]     ContentDisp
  RVal  : HRes HVal1 [<]     String

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
  hfield b HNam ([<m]:>HMap:<n) = dput HMap [<insert n b m]
  hfield _ st   sx              = derr HErr sx st

  meddesc : MediaDesc -> StateAct q HState HSz
  meddesc md HAcc sx t = dput HPar (sx:<md:>HAccD:<[<]) t
  meddesc md st   sx t = derr HErr sx st t

  medtype : MediaType -> StateAct q HState HSz
  medtype m HMT sx t = dput HPar ([<m]:>HMT1:<[<]) t
  medtype m st  sx t = derr HErr sx st t

  condisp : ByteString -> StateAct q HState HSz
  condisp bs HCD sx t = dput HPar ([<toString bs]:>HCD1:<[<]) t
  condisp _  st  sx t = derr HErr sx st t

  hendpar : StateAct q HState HSz
  hendpar HPar (sx:<sd:<md:>HAccD:<sp) = dput HAcc $ sx:<(sd:<MR md (sp<>>[]))
  hendpar st   sx                      = derr HErr sx st

  pname : String -> StateAct q HState HSz
  pname s HPar sx = dput HParN (sx:<s)
  pname s st   sx = derr HErr sx st

  pvalue : String -> StateAct q HState HSz
  pvalue v HParN (sx:<sp:<n) = dput HPar $ sx:<(sp:<P n v)
  pvalue v HVal1 sx          = dput HVal [<v]
  pvalue v st    sx          = derr HErr sx st

  qval : Double -> StateAct q HState HSz
  qval v HPar (sx:<sp) = dput HPar $ sx:<(sp:<Q v)
  qval v st   sx       = derr HErr sx st

  %inline
  hstr : String -> StateAct q HState HSz
  hstr = pvalue

spaced : Steps q HSz SK -> DFA q HSz SK
spaced ss = dfa $ [ignore' (plus WSP)] ++ ss

headerTrans : Lex1 q HSz SK
headerTrans =
  lex1
    [ entry HMap $ dfa [string token $ dpush HNam . toUpper, step' CRLF HEnd]
    , entry HNam $ dfa [step' ':' HField]
    , entry HAcc $ spaced
        [ step' ',' HAcc
        , step "*/*" $ dact (meddesc MDAny)
        , bytes (token >> "/*") $ dact . meddesc . mdstar
        , bytes (token >> "/" >> token) $ dact . meddesc . md
        ]
    , entry HPar $ spaced [step' ';' HParS, step ',' $ dact hendpar]
    , entry HParS $ spaced
        [ step' ';' HParS
        , step ',' $ dact hendpar
        , step' (like "q=") HParQ
        , string token $ dact . pname
        ]
    , entry HParN $ dfa [step' '=' HVal1]
    , entry HParQ $ dfa
        [ step "1." $ dact $ qval 1.0
        , step "0." $ dact $ qval 0.0
        , bytes qvalue $ dact . qval . cast . toString
        ]
    , entry HVal1 $ dfa [string token $ dact . pvalue, opn' '"' HStr]
    , entry HNat $ dfa [bytes (plus digit) $ \bs => dput HNat [<cast $ integer bs]]
    , entry HMT $ dfa [bytes (token >> "/" >> token) $ dact . medtype . mt]
    , entry HCD $ dfa [bytes token $ dact . condisp]
    , entry HStr $ dfa
        [ closeStr '"' $ dact . hstr
        , string qdtext $ pushStr HStr
        , bytes quotedPair $ pushStr HStr . toString . drop 1
        ]
    , entry HField $ spaced [bytes field $ dact . hfield . trim]
    ]

headerErr : Arr32 HSz (SK q -> F1 q (BBErr Void))
headerErr = errs []

end : HRes st x t -> HState ts -> Stack b HState ts -> Maybe t
end RMap  HMap   [<m]                 = Just m
end RAcc  HAcc   [<sm]                = Just $ sm<>>[]
end RAcc  HAccD  [<sm,d]              = Just $ sm<>>[MR d []]
end RVal  HVal   [<s]                 = Just s
end RConL HNat   [<n]                 = Just n
end RConT HMT1   [<m]                 = Just $ CT m []
end RConD HCD1   [<v]                 = Just $ CD v []
end RAcc  HPar   ([<sm,d]:>HAccD:<sp) = Just $ sm<>>[MR d $ sp<>>[]]
end RConT HPar   ([<m]:>HMT1:<sp)     = Just $ CT m (sp <>>[])
end RConD HPar   ([<s]:>HCD1:<sp)     = Just $ CD s (sp <>>[])
end _     _      _                    = Nothing

headerEOI : HRes st x v -> Index HSz -> SK q -> F1 q (Either (BBErr Void) v)
headerEOI res sk s t =
  let (x:>st) # t := read1 s.stack_ t
      Nothing     := end res st x | Just v => Right v # t
   in arrFail SK headerErr sk s t

public export
header : {st : _} -> {x : _} -> HRes st x t -> P1 q (BBErr Void) t
header res =
  P (cast st) (init $ x:>st) headerTrans (\x => (Nothing #))
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
