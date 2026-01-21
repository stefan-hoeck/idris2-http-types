module HTTP.API.Encode

import Data.List.Quantifiers as L
import Data.Vect
import public Data.ByteString
import public HTTP.Header.Types
import JSON.Simple

%default total

||| An interface for encode a value as raw bytes
|||
||| This is used for encoding values in URL paths and query strings
||| but - in general - not for encoding value in the message body.
||| Use `EncodeVia` for that.
public export
interface Encode (0 a : Type) where
  encode : a -> ByteString

||| An interface for encoding values as a list of bytestrings.
public export
interface EncodeMany (0 a : Type) where
  encodeMany : a -> List ByteString

export %inline
Encode a => EncodeMany a where
  encodeMany v = [encode v]

encodeHL :
     {auto all : L.All.All (EncodeMany . f) ts}
  -> L.All.All f ts
  -> List ByteString
encodeHL           []      = []
encodeHL @{_ :: _} (x::xs) = encodeMany x ++ encodeHL xs

export %inline
L.All.All (EncodeMany . f) ts => EncodeMany (L.All.All f ts) where
  encodeMany = encodeHL

encodeList : EncodeMany t => List t -> List ByteString
encodeList []      = []
encodeList (x::xs) = encodeMany x ++ encodeList xs

export %inline
EncodeMany a => EncodeMany (Vect n a) where
  encodeMany = encodeList . toList

export %inline
EncodeMany a => EncodeMany (List a) where
  encodeMany = encodeList

export %inline
EncodeMany a => EncodeMany (SnocList a) where
  encodeMany = encodeList . (<>> [])

--------------------------------------------------------------------------------
-- EncodeVia interface
--------------------------------------------------------------------------------

public export
0 Text : Type
Text = String

public export
0 Octett : Type
Octett = ByteString

public export
interface EncodeVia (0 from, to : Type) where
  encodeAs : from -> to
  toBytes  : to -> List ByteString
  mediaType : MediaType

export %inline
encodeVia : (v : f) -> EncodeVia f t -> List ByteString
encodeVia v c = toBytes @{c} $ encodeAs @{c} v

export %inline
Interpolation a => EncodeVia a String where
  encodeAs  = interpolate
  toBytes   = pure . fromString
  mediaType = MT "test" "plain"

export %inline
Cast a ByteString => EncodeVia a ByteString where
  encodeAs  = cast
  toBytes   = pure
  mediaType = MT "application" "octett-stream"

export %inline
ToJSON a => EncodeVia a JSON where
  encodeAs  = toJSON
  toBytes   = pure . fromString . show
  mediaType = MT "application" "json"

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

export %inline
Encode ByteString where encode = id

export %inline
Encode String where encode = fromString

export
Encode Nat where
  encode = encode . show

export
Encode Integer where
  encode = encode. show

export
Encode Bits8 where
  encode = encode. show

export
Encode Bits16 where
  encode = encode. show

export
Encode Bits32 where
  encode = encode. show

export
Encode Bits64 where
  encode = encode. show

export
Encode Int8 where
  encode = encode. show

export
Encode Int16 where
  encode = encode. show

export
Encode Int32 where
  encode = encode. show

export
Encode Int64 where
  encode = encode. show

export
Encode Double where
  encode = encode. show
