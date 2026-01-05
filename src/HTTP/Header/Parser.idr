module HTTP.Header.Parser

import Data.Buffer
import Data.SortedMap
import Text.ILex
import Text.ILex.Derive

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- Charsets
--------------------------------------------------------------------------------

tokenChar : Set32
tokenChar = chars "!#$%&'*+-.^_`|~"

vchar : Set32
vchar = range $ range 0x21 0x7e

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

export
crlf : RExp True
crlf = "\r\n"

export
wsp : RExp True
wsp = oneof [' ', '\t']

export
token : RExp True
token = plus (alphaNum <|> Ch tokenChar)

export
eol : RExp True
eol = star wsp >> crlf

export
fieldValue : RExp True
fieldValue = plus (Ch vchar <|> wsp)

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
    , E HEnd $ dfa [newline' crlf HName]
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
data Encoding = ASCII | ISO_8859_1 | UTF_8

public export
record HFValue where
  constructor HFP
  encoding : Encoding
  language : Maybe ByteString
  value    : String

public export
record FPart where
  constructor FP
  name   : ByteString
  enc    : Encoding
  lang   : Maybe ByteString
  params : SortedMap ByteString HFValue

init : FPart
init = FP "" ASCII Nothing empty
