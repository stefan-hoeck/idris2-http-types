module HTTP.Parser.Header

import Data.Buffer
import Data.SortedMap
import Derive.Prelude
import HTTP.Header.Types
import HTTP.Parser.Util
import Syntax.T1
import Text.ILex.DStack

%default total
%hide Data.Linear.(.)
%language ElabReflection

md : ByteString -> MediaDesc
md bs =
 let (x,y) := break (47 ==) bs
  in MD (toString x) (toString $ drop 1 y)

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
  HParEq  : HState [Void]
  HAcc    : HState [SnocList MediaRange]
  HAccD   : HState [MediaDesc,SnocList MediaRange]
  HOther  : HState [Void]
  HStr    : HState [Void]
  HEnd    : HState [Void]
  HErr    : HState []

%runElab deriveIndexed "HState" [Show,ConIndex]

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
  hins : HeaderVal -> StateAct q HState HSz
  hins v HNam (n::HMap:>m::t) = dput HMap (insert n v m::t)
  hins v st   x               = derr HErr st x

  hother : ByteString -> StateAct q HState HSz
  hother = hins . Other . toString . trimRight

  meddesc : MediaDesc -> StateAct q HState HSz
  meddesc md HAcc x t = dput HPar ([<]::HAccD:>md::x) t
  meddesc md st   x t = derr HErr st x t

  hcolon : StateAct q HState HSz
  hcolon HNam x@(s::_) t =
    case s of
      "ACCEPT" => dput HAcc ([<]::HNam:>x) t
      _        => cast HOther # t
  hcolon st x t = derr HErr st x t

  hend : StateAct q HState HSz
  hend HAcc  (sm::st:>x)    = hins (Accept $ sm <>> []) st x
  hend HAccD (d::sm::st:>x) = hins (Accept $ sm <>> [MR d []]) st x
  hend HPar  (sp::HAccD:>d::sm::st:>x) =
    hins (Accept $ sm <>> [MR d $ sp<>>[]]) st x
  hend st    x              = derr HErr st x

  hendpar : StateAct q HState HSz
  hendpar HPar (sp::HAccD:>md::sd::x) = dput HAcc $ (sd:<MR md (sp<>>[]))::x
  hendpar st   x                      = derr HErr st x

  pname : String -> StateAct q HState HSz
  pname s HPar x = dput HParN (s::x)
  pname s st   x = derr HErr st x

  pvalue : String -> StateAct q HState HSz
  pvalue v HParN (n::sp::x) = dput HPar $ (sp:<P n v)::x
  pvalue v st    x          = derr HErr st x

  qval : Double -> StateAct q HState HSz
  qval v HPar (sp::x) = dput HPar $ (sp:<Q v)::x
  qval v st   x       = derr HErr st x

  hstr : String -> StateAct q HState HSz
  hstr s st x = pvalue s st x

spaced : HState ts -> Steps q HSz SK -> DFA q HSz SK
spaced r ss =
  dfa $
    [ newline CRLF $ dact hend
    , conv' (plus WSP) r
    ] ++ ss

headerTrans : Lex1 q HSz SK
headerTrans =
  lex1
    [ entry HMap $ dfa [read token $ dpush HNam . toUpper, newline' CRLF HEnd]
    , entry HNam $ dfa [cexpr ':' $ dact hcolon]
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
    , entry HParN $ dfa [cexpr' '=' HParEq]
    , entry HParQ $ dfa
        [ cexpr "1." $ dact $ qval 1.0
        , cexpr "0." $ dact $ qval 0.0
        , conv qvalue $ dact . qval . cast . toString
        ]
    , entry HParEq $ dfa [read token $ dact . pvalue, copen' '"' HStr]
    , entry HStr $ dfa
        [ ccloseStr '"' $ dact . hstr
        , read qdtext $ pushStr HStr
        , conv quotedPair $ pushStr HStr . toString . drop 1
        ]
    , entry HOther $ spaced HOther [convline field $ dact . hother]
    ]

headerErr : Arr32 HSz (SK q -> F1 q (BoundedErr Void))
headerErr = errs []

headerEOI : Index HSz -> SK q -> F1 q (Either (BoundedErr Void) HeaderMap)
headerEOI sk s t =
  if sk == cast HEnd
     then let (HMap:>[m]) # t := getStack t | _ # t => arrFail SK headerErr sk s t
           in Right m # t
     else arrFail SK headerErr sk s t

export
header : P1 q (BoundedErr Void) HSz SK HeaderMap
header = P (cast HMap) (init $ HMap:>[empty]) headerTrans (\x => (Nothing #)) headerErr headerEOI

export
testHeader : String -> IO ()
testHeader s =
  case parseString header Virtual s of
    Left x => putStrLn "\{x}"
    Right hs => traverse_ printLn (kvList hs)

--   Accept-Encoding = [ ( codings [ weight ] ) *( OWS "," OWS ( codings [
--    weight ] ) ) ]
--   Accept-Language = [ ( language-range [ weight ] ) *( OWS "," OWS (
--    language-range [ weight ] ) ) ]
--   Accept-Ranges = acceptable-ranges
--   Allow = [ method *( OWS "," OWS method ) ]
--   Authentication-Info = [ auth-param *( OWS "," OWS auth-param ) ]
--   Authorization = credentials
--
--   Connection = [ connection-option *( OWS "," OWS connection-option )
--    ]
--   Content-Encoding = [ content-coding *( OWS "," OWS content-coding )
--    ]
--   Content-Language = [ language-tag *( OWS "," OWS language-tag ) ]
--   Content-Length = 1*DIGIT
--   Content-Location = absolute-URI / partial-URI
--   Content-Range = range-unit SP ( range-resp / unsatisfied-range )
--   Content-Type = media-type
--
--   Date = HTTP-date
--
--   ETag = entity-tag
--   Expect = [ expectation *( OWS "," OWS expectation ) ]
--
--   From = mailbox
--
--   GMT = %x47.4D.54 ; GMT
--
--   HTTP-date = IMF-fixdate / obs-date
--   Host = uri-host [ ":" port ]
--
--   IMF-fixdate = day-name "," SP date1 SP time-of-day SP GMT
--   If-Match = "*" / [ entity-tag *( OWS "," OWS entity-tag ) ]
--   If-Modified-Since = HTTP-date
--   If-None-Match = "*" / [ entity-tag *( OWS "," OWS entity-tag ) ]
--   If-Range = entity-tag / HTTP-date
--   If-Unmodified-Since = HTTP-date
--
--   Last-Modified = HTTP-date
--   Location = URI-reference
--
--   Max-Forwards = 1*DIGIT
--
--   Proxy-Authenticate = [ challenge *( OWS "," OWS challenge ) ]
--   Proxy-Authentication-Info = [ auth-param *( OWS "," OWS auth-param )
--    ]
--   Proxy-Authorization = credentials
--
--   RWS = 1*( SP / HTAB )
--   Range = ranges-specifier
--   Referer = absolute-URI / partial-URI
--   Retry-After = HTTP-date / delay-seconds
--
--   Server = product *( RWS ( product / comment ) )
--
--   TE = [ t-codings *( OWS "," OWS t-codings ) ]
--   Trailer = [ field-name *( OWS "," OWS field-name ) ]
--
--   URI-reference = <URI-reference, see [URI], Section 4.1>
--   Upgrade = [ protocol *( OWS "," OWS protocol ) ]
--   User-Agent = product *( RWS ( product / comment ) )
--
--   Vary = [ ( "*" / field-name ) *( OWS "," OWS ( "*" / field-name ) )
--    ]
--   Via = [ ( received-protocol RWS received-by [ RWS comment ] ) *( OWS
--    "," OWS ( received-protocol RWS received-by [ RWS comment ] ) ) ]
--
--   WWW-Authenticate = [ challenge *( OWS "," OWS challenge ) ]
--
--   absolute-URI = <absolute-URI, see [URI], Section 4.3>
--   absolute-path = 1*( "/" segment )
--   acceptable-ranges = range-unit *( OWS "," OWS range-unit )
--   asctime-date = day-name SP date3 SP time-of-day SP year
--   auth-param = token BWS "=" BWS ( token / quoted-string )
--   authority = <authority, see [URI], Section 3.2>
--
--   challenge = auth-scheme [ 1*SP ( token68 / [ auth-param *( OWS ","
--    OWS auth-param ) ] ) ]
--   codings = content-coding / "identity" / "*"
--   comment = "(" *( ctext / quoted-pair / comment ) ")"
--   complete-length = 1*DIGIT
--   credentials = auth-scheme [ 1*SP ( token68 / [ auth-param *( OWS ","
--    OWS auth-param ) ] ) ]
--   ctext = HTAB / SP / %x21-27 ; '!'-'''
--    / %x2A-5B ; '*'-'['
--    / %x5D-7E ; ']'-'~'
--    / obs-text
--
--   date1 = day SP month SP year
--   date2 = day "-" month "-" 2DIGIT
--   date3 = month SP ( 2DIGIT / ( SP DIGIT ) )
--   day = 2DIGIT
--   delay-seconds = 1*DIGIT
--
--   entity-tag = [ weak ] opaque-tag
--   etagc = "!" / %x23-7E ; '#'-'~'
--    / obs-text
--   expectation = token [ "=" ( token / quoted-string ) parameters ]
--
--   field-content = field-vchar [ 1*( SP / HTAB / field-vchar )
--    field-vchar ]
--   field-value = *field-content
--   field-vchar = VCHAR / obs-text
--   first-pos = 1*DIGIT
--
--   http-URI = "http://" authority path-abempty [ "?" query ]
--   https-URI = "https://" authority path-abempty [ "?" query ]
--
--   incl-range = first-pos "-" last-pos
--   int-range = first-pos "-" [ last-pos ]
--
--   language-range = <language-range, see [RFC4647], Section 2.1>
--   language-tag = <Language-Tag, see [RFC5646], Section 2.1>
--   last-pos = 1*DIGIT
--
--   mailbox = <mailbox, see [RFC5322], Section 3.4>
--   media-range = ( "*/*" / ( type "/*" ) / ( type "/" subtype ) )
--    parameters
--   media-type = type "/" subtype parameters
--
--   opaque-tag = DQUOTE *etagc DQUOTE
--   other-range = 1*( %x21-2B ; '!'-'+'
--    / %x2D-7E ; '-'-'~'
--    )
--
--   partial-URI = relative-part [ "?" query ]
--   path-abempty = <path-abempty, see [URI], Section 3.3>
--   port = <port, see [URI], Section 3.2.3>
--
--   query = <query, see [URI], Section 3.4>
--   quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE
--
--   range-resp = incl-range "/" ( complete-length / "*" )
--   range-set = range-spec *( OWS "," OWS range-spec )
--   range-spec = int-range / suffix-range / other-range
--   ranges-specifier = range-unit "=" range-set
--   received-by = pseudonym [ ":" port ]
--   received-protocol = [ protocol-name "/" ] protocol-version
--   relative-part = <relative-part, see [URI], Section 4.2>
--   rfc850-date = day-name-l "," SP date2 SP time-of-day SP GMT
--
--   segment = <segment, see [URI], Section 3.3>
--   suffix-length = 1*DIGIT
--   suffix-range = "-" suffix-length
--
--   t-codings = "trailers" / ( transfer-coding [ weight ] )
--   token68 = 1*( ALPHA / DIGIT / "-" / "." / "_" / "~" / "+" / "/" )
--    *"="
--   transfer-coding = token *( OWS ";" OWS transfer-parameter )
--   transfer-parameter = token BWS "=" BWS ( token / quoted-string )
--
--   unsatisfied-range = "*/" complete-length
--   uri-host = <host, see [URI], Section 3.2.2>
--
--   weak = %x57.2F ; W/

--------------------------------------------------------------------------------
-- Proofs
--------------------------------------------------------------------------------

inBoundsHState HMap    = Refl
inBoundsHState HNam    = Refl
inBoundsHState HPar    = Refl
inBoundsHState HParQ   = Refl
inBoundsHState HParS   = Refl
inBoundsHState HParN   = Refl
inBoundsHState HParEq  = Refl
inBoundsHState HAcc    = Refl
inBoundsHState HAccD   = Refl
inBoundsHState HOther  = Refl
inBoundsHState HStr    = Refl
inBoundsHState HEnd    = Refl
inBoundsHState HErr    = Refl
