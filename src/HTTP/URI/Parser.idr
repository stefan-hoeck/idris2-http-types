module HTTP.URI.Parser

import Derive.Prelude
import Data.Bits
import Data.ByteVect as BV
import Text.ILex
import Text.ILex.Derive

%default total
%language ElabReflection

public export %inline
byte_percent : Bits8
byte_percent = 37

public export %inline
byte_equals : Bits8
byte_equals = 61

public export %inline
byte_questionmark : Bits8
byte_questionmark = 63

public export %inline
byte_ampersand : Bits8
byte_ampersand = 38

hex : Bits8 -> Bits8
hex b = if b < 10 then byte_0 + b else byte_A + b - 10

escape : Bits8 -> ByteString
escape b = pack [byte_percent, hex (shiftR b 4 .&. 0xf), hex (b .&. 0xf)]

||| URI-escapes all bytes for which the given predicate does
||| not return `True`.
|||
||| An escaped byte is represented as two hexadecimal digits prefixed by
||| a "percent" character ('%').
export
uriEscape : (Bits8 -> Bool) -> ByteString -> ByteString
uriEscape p = go [<]
  where
    go : SnocList ByteString -> ByteString -> ByteString
    go sx bs =
      case break p bs of
        (pre, BS 0 _)      => fastConcat $ sx <>> [pre]
        (pre, BS (S k) bv) =>
          let b := head bv
           in go (sx :< pre :< escape b) (assert_smaller bs $ BS k $ tail bv)

||| Converts all escape sequences (see `uriEscape`) in a byte string
||| to the corresponding bytes.
|||
||| Note: This is an internal function and should only be invoked
|||       with correctly escaped byte strings.
export
uriUnescape : ByteString -> ByteString
uriUnescape = go [<]
  where
    go : SnocList ByteString -> ByteString -> ByteString
    go sx bs =
      case break (byte_percent ==) bs of
        (pre, bs2@(BS (S (S (S k))) bv)) =>
          let b := cast $ 16 * hexdigit (at bv 1) + hexdigit (at bv 2)
           in go (sx :< pre :< singleton b) (assert_smaller bs $ drop 3 bs2)
        (pre, _) => fastConcat $ sx <>> [pre]

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

-- <|> oneof ['-','.','_','~']
isUnreserved : Bits8 -> Bool
isUnreserved 45  = True -- '-'
isUnreserved 46  = True -- '.'
isUnreserved 95  = True -- '_'
isUnreserved 126 = True -- '~'
isUnreserved b   = isAlphaNum b

-- subDelims = oneof ['!','$','&','\'','(',')','*','+',',',';','=']
isSubDelims : Bits8 -> Bool
isSubDelims 33 = True -- '!'
isSubDelims 36 = True -- '$'
isSubDelims 38 = True -- '&'
isSubDelims 39 = True -- '\''
isSubDelims 40 = True -- '('
isSubDelims 41 = True -- ')'
isSubDelims 42 = True -- '*'
isSubDelims 43 = True -- '+'
isSubDelims 44 = True -- ','
isSubDelims 59 = True -- ';'
isSubDelims 61 = True -- '='
isSubDelims _  = False

export
ispchar : Bits8 -> Bool
ispchar 58 = True -- ':'
ispchar 64 = True -- '@'
ispchar c  = isUnreserved c || isSubDelims c

export
isAuthByte : Bits8 -> Bool
isAuthByte = ispchar

export
isQueryByte : Bits8 -> Bool
isQueryByte 47 = True -- '/'
isQueryByte 63 = True -- '?'
isQueryByte 38 = False -- '&'
isQueryByte c  = ispchar c

export
isQueryNameByte : Bits8 -> Bool
isQueryNameByte 61 = False -- '='
isQueryNameByte c  = isQueryByte c

export %inline
isFragmentByte : Bits8 -> Bool
isFragmentByte 47 = True -- '/'
isFragmentByte 63 = True -- '?'
isFragmentByte c  = ispchar c

--------------------------------------------------------------------------------
-- Regular Expressions
--------------------------------------------------------------------------------

scheme : RExp True
scheme = alpha >> star (alphaNum <|> oneof ['+','-','.'])

subDelims : RExp True
subDelims = oneof ['!','$','&','\'','(',')','*','+',',',';','=']

genDelims : RExp True
genDelims = oneof [':','/','?','#','[',']','@']

unreserved : RExp True
unreserved = alphaNum <|> oneof ['-','.','_','~']

reserved : RExp True
reserved = genDelims <|> subDelims

pctEncoded : RExp True
pctEncoded = '%' >> hexdigit >> hexdigit

pchar : RExp True
pchar = unreserved <|> pctEncoded <|> subDelims <|> oneof [':','@']

fragment : RExp True
fragment = '#' >> star (pchar <|> oneof ['/','?'])

query : RExp True
query = '?' >> star (pchar <|> oneof ['/','?'])

segment : RExp False
segment = star pchar

segmentNz : RExp True
segmentNz   = plus pchar

segmentNzNc : RExp True
segmentNzNc = plus $ unreserved <|> pctEncoded <|> subDelims <|> '@'

regName : RExp False
regName = star $ unreserved <|> pctEncoded <|> subDelims

decOctet : RExp True
decOctet =
      digit
  <|> (posdigit >> digit)
  <|> ('1' >> digit >> digit)
  <|> ('2' >> range '0' '4' >> digit)
  <|> ("25" >> range '0' '5')

ip4address : RExp True
ip4address = decOctet >> repeat 3 ("." >> decOctet)

h16 : RExp True
h16 = repeatRange 1 4 hexdigit

ls32 : RExp True
ls32 = (h16 >> ":" >> h16) <|> ip4address

ip6address : RExp True
ip6address =
      (                                                     repeat 6 (h16 >> ':') >> ls32)
  <|> (                                             "::" >> repeat 5 (h16 >> ':') >> ls32)
  <|> (opt                                  h16  >> "::" >> repeat 4 (h16 >> ':') >> ls32)
  <|> (opt (repeatRange 0 1 (h16 >> ':') >> h16) >> "::" >> repeat 3 (h16 >> ':') >> ls32)
  <|> (opt (repeatRange 0 2 (h16 >> ':') >> h16) >> "::" >> repeat 2 (h16 >> ':') >> ls32)
  <|> (opt (repeatRange 0 3 (h16 >> ':') >> h16) >> "::" >>           h16 >> ':'  >> ls32)
  <|> (opt (repeatRange 0 4 (h16 >> ':') >> h16) >> "::"                          >> ls32)
  <|> (opt (repeatRange 0 5 (h16 >> ':') >> h16) >> "::"                          >> h16)
  <|> (opt (repeatRange 0 6 (h16 >> ':') >> h16) >> "::")

ipFuture : RExp True
ipFuture = 'v' >> plus hexdigit >> '.' >> plus (unreserved <|> subDelims <|> ':')

ipLiteral : RExp True
ipLiteral = '[' >> (ip6address <|> ipFuture) >> ']'

host : RExp False
host = ipLiteral <|> ip4address <|> regName

userinfo : RExp False
userinfo = star (unreserved <|> pctEncoded <|> subDelims <|> ':')

authority : RExp True
authority = "//" >> opt (userinfo >> '@') >> host >> opt (':' >> star digit)

--------------------------------------------------------------------------------
-- Parser State
--------------------------------------------------------------------------------

%runElab deriveParserState "USz" "UST"
  ["Init", "Hier", "Segments", "Fragment", "End"]

public export
record Part where
  constructor P
  sch  : Maybe ByteString
  auth : Maybe ByteString
  abs  : Bool
  segs : SnocList ByteString
  ques : Maybe ByteString
  frag : Maybe ByteString

%runElab derive "Part" [Show,Eq]

pinit : Part
pinit = P Nothing Nothing False [<] Nothing Nothing

setScheme : ByteString -> Part -> Part
setScheme bs = {sch := Just $ dropEnd 1 bs}

setQuery : ByteString -> Part -> Part
setQuery bs = {ques := Just $ uriUnescape (drop 1 bs)}

setAuth : ByteString -> Part -> Part
setAuth bs = {auth := Just $ uriUnescape $ drop 2 bs}

absoluteSegment : ByteString -> Part -> Part
absoluteSegment bs = {abs := True, segs := [<uriUnescape $ drop 1 bs]}

rootSegment : ByteString -> Part -> Part
rootSegment bs = {segs := [<uriUnescape bs]}

addSegment : ByteString -> Part -> Part
addSegment bs = {segs $= (:< uriUnescape (drop 1 bs))}

setFragment : ByteString -> Part -> Part
setFragment bs = {frag := Just $ uriUnescape (drop 1 bs)}

public export
0 SK : Type -> Type
SK = Stack Void Part USz

%inline
upd : SK q => UST -> (Part -> Part) -> F1 q UST
upd u f = modStackAs SK f u

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

init : DFA q USz SK
init =
  dfa
    [ conv (scheme >> ':') $ upd Hier . setScheme
    , conv authority $ upd Segments . setAuth
    , conv ('/' >> opt segmentNz) $ upd Segments . absoluteSegment
    , conv segmentNzNc $ upd Segments . rootSegment
    , conv query $ upd Fragment . setQuery
    , conv fragment $ upd End . setFragment
    ]

hier : DFA q USz SK
hier =
  dfa
    [ conv authority $ upd Segments . setAuth
    , conv ('/' >> opt segmentNz) $ upd Segments . absoluteSegment
    , conv segmentNz $ upd Segments . rootSegment
    , conv query $ upd Fragment . setQuery
    , conv fragment $ upd End . setFragment
    ]

segments : DFA q USz SK
segments =
  dfa
    [ conv ('/' >> segment) $ upd Segments . addSegment
    , conv query $ upd Fragment . setQuery
    , conv fragment $ upd End . setFragment
    ]

uriTrans : Lex1 q USz SK
uriTrans =
  lex1
    [ E Init init
    , E Hier hier
    , E Segments segments
    , E Fragment $ dfa [conv fragment $ upd End . setFragment]
    ]

uriErr : Arr32 USz (SK q -> F1 q (BoundedErr Void))
uriErr = arr32 USz (unexpected []) []

uriEOI : UST -> SK q -> F1 q (Either (BoundedErr Void) Part)
uriEOI sk s t = let v # t := getStack t in Right v # t

export
uri : P1 q (BoundedErr Void) USz SK Part
uri = P Init (init pinit) uriTrans (\x => (Nothing #)) uriErr uriEOI
