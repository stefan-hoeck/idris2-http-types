module HTTP.API.Decode

import Data.List.Quantifiers as L
import Derive.Prelude
import HTTP.FormData
import HTTP.Header.Types
import HTTP.RequestErr
import HTTP.Status
import HTTP.URI
import JSON.Simple
import JSON.Simple.Derive

%default total
%language ElabReflection

||| Error type that occurs when decoding a value or other piece of
||| information.
public export
data DecodeErr : Type where
  ||| A `ReadErr` typically occurs when reading a single value
  ||| from a string or bytestring.
  |||
  ||| @type    : String description of the type we tried to read
  ||| @value   : The string from which the value should be read
  ||| @details : Additional information about why reading the value failed.
  ReadErr    : (type, value : String) -> (details : String) -> DecodeErr

  ||| A `ContentErr` is - in general - a more technical error that happend
  ||| when parsing the body of a message. The `details` field typically
  ||| holds the detailed description from the parser about what went
  ||| actually wrong.
  ContentErr : (type : String) -> (details : String) -> DecodeErr

  ||| An arbitrary custom error message.
  Msg        : (message : String) -> DecodeErr

%runElab derive "DecodeErr" [Show,Eq,FromJSON,ToJSON]

||| Utility constructor for `ReadErr`.
export %inline
readErr : (type : String) -> (value : ByteString) -> DecodeErr
readErr type value = ReadErr type (toString value) ""

||| Utility constructor for `ContentErr`.
export %inline
contentErr : (type : String) -> Interpolation a => a -> DecodeErr
contentErr type = ContentErr type . interpolate

||| Adjusts the `type` field of a decode error.
export
setType : String -> DecodeErr -> DecodeErr
setType t (ReadErr _ v d)  = ReadErr t v d
setType t (ContentErr _ d) = ContentErr t d
setType t (Msg m)          = Msg m

||| Adjusts the `type` field of a decode error.
export
setValue : String -> DecodeErr -> DecodeErr
setValue v (ReadErr t _ d)  = ReadErr t v d
setValue _ err              = err

||| Adjusts the `message` or `details` field of a decode error.
export
modMsg : (String -> String) -> DecodeErr -> DecodeErr
modMsg f (ReadErr t v d)  = ReadErr t v (f d)
modMsg f (ContentErr t d) = ContentErr t (f d)
modMsg f (Msg m)          = Msg (f m)

--------------------------------------------------------------------------------
-- Decode Interface
--------------------------------------------------------------------------------

||| An interface for decoding value from a sequence of raw bytes.
public export
interface Decode (0 a : Type) where
  decode : ByteString -> Either DecodeErr a

||| Utiliy alias for `decode` that allows to explicitly specify the
||| target type.
public export %inline
decodeAs : (0 a : Type) -> Decode a => ByteString -> Either DecodeErr a
decodeAs _ = decode

||| An interface for decoding values by reading a prefix
||| of a list of bytestrings such as a path in a URL.
public export
interface DecodeMany (0 a : Type) where
  simulateDecode : List ByteString -> Maybe (List ByteString)

  decodeMany : List ByteString -> Either DecodeErr (List ByteString, a)

export
Decode a => DecodeMany a where
  simulateDecode []      = Nothing
  simulateDecode (b::bs) = Just bs

  decodeMany []      = Left (Msg "Unexpected end of URL path")
  decodeMany (b::bs) = (bs,) <$> decode b

export
decodeAll :
     SnocList a
  -> Decode a
  -> List ByteString
  -> Either DecodeErr (List ByteString,SnocList a)
decodeAll sx d []        = Right ([],sx)
decodeAll sx d (x :: xs) =
  case decode @{d} x of
    Right v  => decodeAll (sx:<v) d xs
    Left err => Left err

export
Decode a => DecodeMany (SnocList a) where
  simulateDecode bs = Just []
  decodeMany = decodeAll [<] %search

export
Decode a => DecodeMany (List a) where
  simulateDecode bs = Just []
  decodeMany bs = map (<>> []) <$> decodeAll [<] %search bs

simulateHL :
     L.All.All (DecodeMany . f) ts
  -> List ByteString
  -> Maybe (List ByteString)
simulateHL []       xs = Just xs
simulateHL (x :: y) xs =
  case simulateDecode @{x} xs of
    Nothing  => Nothing
    Just xs2 => simulateHL y xs2

decodeHL :
     L.All.All (DecodeMany . f) ts
  -> List ByteString
  -> Either DecodeErr (List ByteString, L.All.All f ts)
decodeHL []       xs = Right (xs, [])
decodeHL (x :: y) xs =
  case decodeMany @{x} xs of
    Left err       => Left err
    Right (xs2, v) => map (v::) <$> decodeHL y xs2

export %inline
(all : L.All.All (DecodeMany . f) ts) => DecodeMany (L.All.All f ts) where
  simulateDecode = simulateHL all
  decodeMany = decodeHL all

simulateN :
     DecodeMany t
  -> (n : Nat)
  -> List ByteString
  -> Maybe (List ByteString)
simulateN x 0     xs = Just xs
simulateN x (S n) xs =
  case simulateDecode @{x} xs of
    Nothing  => Nothing
    Just xs2 => simulateN x n xs2

decodeN :
     DecodeMany t
  -> (n : Nat)
  -> List ByteString
  -> Either DecodeErr (List ByteString, Vect n t)
decodeN x 0     xs = Right (xs, [])
decodeN x (S n) xs =
  case decodeMany @{x} xs of
    Left err       => Left err
    Right (xs2, v) => map (v::) <$> decodeN x n xs2

export %inline
{n : Nat} -> (x : DecodeMany a) => DecodeMany (Vect n a) where
  simulateDecode = simulateN x n
  decodeMany = decodeN x n

--------------------------------------------------------------------------------
-- DecodeVia
--------------------------------------------------------------------------------

namespace DecodeVia
  public export
  interface DecodeVia (0 from, to : Type) where
    fromBytes  : Parameters -> ByteString -> Either DecodeErr from
    decodeFrom : from -> Either DecodeErr to
    mediaType  : MediaType

export
decodeVia :
     {auto d : DecodeVia from to}
  -> Parameters
  -> ByteString
  -> Either DecodeErr to
decodeVia ps bs = fromBytes @{d} ps bs >>= decodeFrom

public export
interface FromFormData a where
  fromFormData : FormData -> Either DecodeErr a

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

export %inline
Decode ByteString where decode = Right

export %inline
Decode String where decode = Right . toString

export
refinedEither :
     {auto r : Decode a}
  -> (type : String)
  -> (a -> Either String b)
  -> ByteString
  -> Either DecodeErr b
refinedEither t f bs = Prelude.do
  v <- mapFst (setType t) $ decodeAs a bs
  mapFst (ReadErr t (toString bs)) (f v)

export
refined :
     {auto r  : Decode a}
  -> (type    : String)
  -> (details : Lazy String)
  -> (a -> Maybe b)
  -> ByteString
  -> Either DecodeErr b
refined t details f bs = Prelude.do
  v <- mapFst (setType t) $ decodeAs a bs
  case f v of
    Nothing => Left $ ReadErr t (toString bs) details
    Just x  => Right x
