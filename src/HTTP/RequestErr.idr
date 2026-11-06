module HTTP.RequestErr

import Control.Monad.MErr
import Data.Either
import Derive.Prelude
import HTTP.Status
import JSON.Simple
import JSON.Simple.Derive

%default total
%language ElabReflection

public export
record RequestErr where
  constructor RE
  status  : Bits16
  error   : String
  message : String
  details : Either JSON String
  path    : String

%runElab derive "RequestErr" [Show,Eq,FromJSON,ToJSON]

export
requestErr : Status -> RequestErr
requestErr (MkStatus c e) = RE c e "" (Left JNull) ""

export
requestErrMsg : String -> Status -> RequestErr
requestErrMsg m (MkStatus c e) = RE c e m (Left JNull) ""

export
requestErrDetails : Interpolation a => a -> Status -> RequestErr
requestErrDetails v (MkStatus c e) = RE c e "" (Right "\{v}") ""

export
requestErrJSON : ToJSON a => a -> Status -> RequestErr
requestErrJSON v (MkStatus c e) = RE c e "" (Left $ toJSON v) ""

||| Tries to convert a `RequestErr` to an error of type `e`
||| by decoding its `details` field.
export
toJSONErr : FromJSON e => RequestErr -> Maybe e
toJSONErr (RE _ _ _ (Left j)  _) = eitherToMaybe $ fromJSON j
toJSONErr (RE _ _ _ (Right s) _) = eitherToMaybe $ decode s

||| Tries to convert a `RequestErr` to an error of type `e`
||| and throws that in case the conversion succeeds. Otherwise,
||| the original request error is rethrown.
export
decodeErr :
     (0 e : Type)
  -> {auto hr : Has RequestErr es}
  -> {auto ha : Has e es}
  -> {auto fj : FromJSON e}
  -> {auto me : MErr f}
  -> f es a
  -> f es a
decodeErr e act =
  catch RequestErr act >>= \case
    Right v => pure v
    Left x  => maybe (throw x) throw (toJSONErr @{fj} x)
