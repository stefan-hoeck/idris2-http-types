module HTTP.I18n.EN

import HTTP.API.Decode
import HTTP.RequestErr
import IO.Async.Logging
import public HTTP.I18n

%default total

export
[HTTPEN] HTTPLocal where
  endOfURIPath = "unexpected end of URI path"
  floatingPointNumber = "floating point number"
  integer = "integer"
  invalidPath = "invalid URI path"
  jsonValue = "JSON value"
  logLevel = interpolate
  missingBoundary = "invalid form-data header: missing boundary"
  missingFormDataPart p ps = "missing form-data part: \{p} (parts: \{ps})"
  missingHeader h = "missing HTTP header: \{h}"
  missingQueryParameter n = "missing query parameter: '\{n}'"
  missingQueryValue n = "missing query value: '\{n}'"
  myMediaTypeNotAccepted x y = "i provide \{x} but requests accepts only \{y}"
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
