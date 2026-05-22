module HTTP.I18n.EN

import HTTP.API.Decode
import HTTP.RequestErr
import public HTTP.I18n

%default total

export
HTTPLocal where
  floatingPointNumber = "floating point number"
  integer = "integer"
  jsonValue = "JSON value"
  missingBoundary = "invalid form-data header: missing boundary"
  missingFormDataPart p ps = "missing form-data part: \{p} (parts: \{ps})"
  unsignedInteger = "unsigned integer"
  naturalNumber = "natural number"
  outOfBounds a b =
    "Value out of bounds. It should be between \{show a} and \{show b}."

  prettyRequestErr (RE s e m d p) =
    """
    Error Details:
    status  : \{show s}
    error   : \{e}
    message : \{m}
    details : \{d}
    path    : \{p}
    """

  prettyDecodeErr (ReadErr t s d) = "invalid \{t}\{valueString s}"
  prettyDecodeErr (ContentErr t d) = "invalid \{t}"
  prettyDecodeErr (Msg msg) = msg
