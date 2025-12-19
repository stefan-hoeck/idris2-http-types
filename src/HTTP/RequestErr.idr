module HTTP.RequestErr

import Derive.Prelude
import HTTP.Status
import JSON.Simple
import JSON.Simple.Derive

%default total
%language ElabReflection

public export
record RequestErr where
  constructor RE
  status  : Nat
  error   : String
  message : String
  details : String
  path    : String

%runElab derive "RequestErr" [Show,Eq,FromJSON,ToJSON]

export
Interpolation RequestErr where
  interpolate (RE s e m d p) =
    """
    Error Details:
    status  : \{show s}
    error   : \{e}
    message : \{m}
    details : \{d}
    path    : \{p}
    """

export
requestErr : Status -> RequestErr
requestErr (MkStatus c e) = RE c e "" "" ""
