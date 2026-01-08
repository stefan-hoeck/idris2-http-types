module HTTP.Parser.Header

import Data.Buffer
import Data.SortedMap
import HTTP.Parser.Util
import Text.ILex.Derive

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- Charsets
-- (see Appendix A from [RFC 91110](https://www.rfc-editor.org/rfc/rfc9110.txt)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Regular Expressions
-- (see Appendix A from [RFC 91110](https://www.rfc-editor.org/rfc/rfc9110.txt)
--------------------------------------------------------------------------------

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

export
weight : RExp True
weight = OWS >> ';' >> OWS >> like "q=" >> qvalue

--   Accept = [ ( media-range [ weight ] ) *( OWS "," OWS ( media-range [
--    weight ] ) ) ]
--   Accept-Charset = [ ( ( token / "*" ) [ weight ] ) *( OWS "," OWS ( (
--    token / "*" ) [ weight ] ) ) ]
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
--   parameter = parameter-name "=" parameter-value
--   parameter-value = ( token / quoted-string )
--   parameters = *( OWS ";" OWS [ parameter ] )
--   partial-URI = relative-part [ "?" query ]
--   path-abempty = <path-abempty, see [URI], Section 3.3>
--   port = <port, see [URI], Section 3.2.3>
--
--   qdtext = HTAB / SP / "!" / %x23-5B ; '#'-'['
--    / %x5D-7E ; ']'-'~'
--    / obs-text
--   query = <query, see [URI], Section 3.4>
--   quoted-pair = "\" ( HTAB / SP / VCHAR / obs-text )
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
-- Expressions
--------------------------------------------------------------------------------

export
token : RExp True
token = plus (alphaNum <|> Ch tokenChar)

export
eol : RExp True
eol = star WSP >> CRLF

export
fieldValue : RExp True
fieldValue = plus (Ch VCHAR <|> WSP)

export %inline
productVersion : RExp True
productVersion = token

export
product : RExp True
product = token >> opt ('/' >> productVersion)

export %inline
protocolName : RExp True
protocolName = token

export %inline
protocolVersion : RExp True
protocolVersion = token

export %inline
protocol : RExp True
protocol = protocolName >> opt ('/' >> protocolVersion)

export %inline
pseudonym : RExp True
pseudonym = token

export %inline
type : RExp True
type = token

export %inline
subtype : RExp True
subtype = token

export %inline
parameterName : RExp True
parameterName = token

export %inline
rangeUnit : RExp True
rangeUnit = token

export %inline
authScheme : RExp True
authScheme = token

export %inline
connectionOption : RExp True
connectionOption = token

export %inline
contentCoding : RExp True
contentCoding = token

export %inline
fieldName : RExp True
fieldName = token

export %inline
method : RExp True
method = token

--------------------------------------------------------------------------------
-- Headers Parser
--------------------------------------------------------------------------------

%runElab deriveParserState "HSz" "HST"
  ["HName", "HColon", "HVal", "HEnd"]

public export
record HPart where
  constructor HP
  name  : ByteString
  pairs : SortedMap ByteString ByteString

%inline
setName : ByteString -> HPart -> HPart
setName bs = {name := toUpper bs}

%inline
addVal : ByteString -> HPart -> HPart
addVal bs p = {pairs $= insert p.name bs} p

%inline
addEmpty : HPart -> HPart
addEmpty = addVal empty

public export
0 SK : Type -> Type
SK = Stack Void HPart HSz

%inline
upd : SK q => HST -> (HPart -> HPart) -> F1 q HST
upd u f = modStackAs SK f u

hval : DFA q HSz SK
hval =
  dfa
    [ newline eol $ upd HName addEmpty
    , conv fieldValue $ upd HEnd . addVal . trim
    ]

headerTrans : Lex1 q HSz SK
headerTrans =
  lex1
    [ E HName $ dfa [conv token $ upd HColon . setName]
    , E HColon $ dfa [cexpr' ':' HVal]
    , E HVal hval
    , E HEnd $ dfa [newline' CRLF HName]
    ]

headerErr : Arr32 HSz (SK q -> F1 q (BoundedErr Void))
headerErr = arr32 HSz (unexpected []) []

headerEOI :
     HST
  -> SK q
  -> F1 q (Either (BoundedErr Void) $ SortedMap ByteString ByteString)
headerEOI sk s t =
  if sk == HName || sk == HEnd
     then let v # t := getStack t in Right v.pairs # t
     else arrFail SK headerErr sk s t

export
header : P1 q (BoundedErr Void) HSz SK (SortedMap ByteString ByteString)
header =
  P HName (init $ HP "" empty) headerTrans (\x => (Nothing #)) headerErr headerEOI

--------------------------------------------------------------------------------
-- Field Params
--------------------------------------------------------------------------------

%runElab deriveParserState "FSz" "FST"
  ["FName", "FEq", "FVal"]

public export
record HFValue where
  constructor HFP
  language : Maybe ByteString
  value    : String

public export
record FPart where
  constructor FP
  name   : ByteString
  lang   : Maybe ByteString
  params : SortedMap ByteString HFValue

init : FPart
init = FP "" Nothing empty
