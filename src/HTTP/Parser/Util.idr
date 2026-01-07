module HTTP.Parser.Util

import Data.Bits
import public Text.ILex

%default total

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

export
hex : Bits8 -> Bits8
hex b = if b < 10 then byte_0 + b else byte_A + b - 10

export
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
-- Charsets
-- (see Appendix B from [RFC 5234](https://www.rfc-editor.org/rfc/rfc5234.txt)
--------------------------------------------------------------------------------

export %inline
OCTETT : Set8
OCTETT = range (range 0x00 0xff)

export %inline
ALPHA : Set32
ALPHA = alpha

export %inline
BIT : Set32
BIT = range $ range 48 49

export %inline
DIGIT : Set32
DIGIT = range $ range 0x30 0x39

export %inline
HEXDIG : Set32
HEXDIG = DIGIT `union` (range (range 65 70) `union` range (range 97 102))

export %inline
CHAR : Set32
CHAR = range $ range 0x01 0x7f

export %inline
CTL : Set32
CTL = range (range 0x00 0x1f) `union` singleton 0x7f

export %inline
VCHAR : Set32
VCHAR = range $ range 0x21 0x7e

export
tokenChar : Set32
tokenChar = chars "!#$%&'*+-.^_`|~"

--------------------------------------------------------------------------------
-- Regular Expressions
-- (see Appendix B from [RFC 5234](https://www.rfc-editor.org/rfc/rfc5234.txt)
--------------------------------------------------------------------------------

public export %inline
CR : RExp True
CR = Ch $ singleton 0x0d

public export %inline
LF : RExp True
LF = Ch $ singleton 0xa

public export
CRLF : RExp True
CRLF = CR >> LF

public export %inline
DQUOTE : RExp True
DQUOTE = Ch $ singleton 0x22

public export %inline
HTAB : RExp True
HTAB = Ch $ singleton 0x09

public export %inline
SP : RExp True
SP = Ch $ singleton 0x20

public export
WSP : RExp True
WSP = SP <|> HTAB

||| Optional whitespace
export
OWS : RExp False
OWS = star $ SP <|> HTAB

export %inline
BWS : RExp False
BWS = OWS

public export
pctEncoded : RExp True
pctEncoded = '%' >> Ch HEXDIG >> Ch HEXDIG
