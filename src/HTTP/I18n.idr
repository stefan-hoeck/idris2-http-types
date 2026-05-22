module HTTP.I18n

import Data.ByteString
import HTTP.API.Decode
import HTTP.FormData
import HTTP.Header.Types
import HTTP.RequestErr
import HTTP.Status
import JSON.Simple
import Text.ILex

%default total

public export
interface HTTPLocal where
  endOfURIPath : String
  floatingPointNumber : String
  integer : String
  jsonValue : String
  missingBoundary : String
  missingFormDataPart : (part, parts : String) -> String
  missingHeader : String -> String
  missingQueryParameter : String -> String
  missingQueryValue : String -> String
  myMediaTypeNotAccepted : String -> String -> String
  naturalNumber : String
  outOfBounds : Show a => (min,max : a) -> String
  prettyDecodeErr : DecodeErr -> String
  prettyRequestErr : RequestErr -> String
  unsignedInteger : String


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

||| Utility for quoting non-empty strings in decode errors.
export
valueString : String -> String
valueString "" = ""
valueString s  = ": '\{s}'"

dets : DecodeErr -> String
dets (ContentErr _ ds) = ds
dets (ReadErr _ _ ds)  = ds
dets _                 = ""

export
commaSep : List String -> String
commaSep = fastConcat . intersperse ","

export
commaSepI : Interpolation a => List a -> String
commaSepI = commaSep . map interpolate

export
commaSepS : Show a => List a -> String
commaSepS = commaSep . map show

parameters {auto loc : HTTPLocal}

  export %inline
  Interpolation RequestErr where
    interpolate = prettyRequestErr

  export %inline
  Interpolation DecodeErr where
    interpolate = prettyDecodeErr

  export
  decodeErr : Status -> DecodeErr -> RequestErr
  decodeErr s de = {message := "\{de}", details := dets de} (requestErr s)

  export
  bounded :
       (0 a    : Type)
    -> {auto r : Decode a}
    -> {auto o : Ord a}
    -> {auto s : Show a}
    -> {auto c : Cast a b}
    -> (type    : String)
    -> (min,max : a)
    -> ByteString
    -> Either DecodeErr b
  bounded a t min max = refined t (outOfBounds min max) $ \v =>
    if (min <= v && v <= max) then Just (cast v) else Nothing

  export
  Decode Nat where
    decode (BS 0 _) = Left $ readErr naturalNumber empty
    decode bs =
      if all isDigit bs
         then Right (cast $ decimal bs)
         else Left $ readErr naturalNumber bs

  export
  Decode Integer where
    decode (BS 0 _) = Left $ readErr integer empty
    decode bs@(BS (S k) bv) =
      mapFst (setType integer) $ case head bv of
        45 => map (negate . cast) (decodeAs Nat (BS k $ tail bv))
        _  => map cast $ decodeAs Nat bs

  export
  Decode Bits8 where
    decode = bounded Integer unsignedInteger 0 0xff

  export
  Decode Bits16 where
    decode = bounded Integer unsignedInteger 0 0xffff

  export
  Decode Bits32 where
    decode = bounded Integer unsignedInteger 0 0xffff_ffff

  export
  Decode Bits64 where
    decode = bounded Integer unsignedInteger 0 0xffff_ffff_ffff_ffff

  export
  Decode Int8 where
    decode = bounded Integer integer (-0x80) 0x7f

  export
  Decode Int16 where
    decode = bounded Integer integer (-0x8000) 0x7fff

  export
  Decode Int32 where
    decode = bounded Integer integer (-0x8000_0000) 0x7fff_ffff

  export
  Decode Int64 where
    decode = bounded Integer integer (-0x8000_0000_0000_0000) 0x7fff_ffff_ffff_ffff

  export
  Decode Double where
    decode bs =
      case runBytes json bs of
        Right (JDouble x)  => Right x
        Right (JInteger x) => Right $ cast x
        _                  => Left $ readErr floatingPointNumber bs

  parameters {auto fj : FromJSON a}

    export
    DecodeVia JSON a where
      fromBytes _ = mapFst (contentErr jsonValue) . parseBytes json Virtual
      decodeFrom  = mapFst (contentErr jsonValue . JErr) . fromJSON
      mediaType   = MT "application" "json"

  parameters {auto fd : FromFormData a}

    export
    DecodeVia FormData a where
      fromBytes ps bs =
        case parameter "boundary" ps of
          Just b  => Right $ multipart (fromString b) bs
          Nothing => Left $ Msg missingBoundary
      decodeFrom      = fromFormData
      mediaType       = MT "multipart" "form-data"

  export
  getFDBytes : String -> FormData -> Either DecodeErr ByteString
  getFDBytes s xs =
    case find ((s ==) . name) xs of
      Nothing => Left $ Msg $ missingFormDataPart s (commaSep $ map name xs)
      Just p  => Right p.content
