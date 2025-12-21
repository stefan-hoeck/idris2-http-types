module HTTP.Request

import Control.Monad.MErr
import HTTP.API
import Text.ILex

%default total

||| A request sent from the client listing the SCGI headers provided by
||| the proxy server, the total content size, and an IO action for
||| streaming the content.
public export
record Request where
  [noHints]
  constructor RQ

  ||| The HTTP method used for the request
  method      : Method

  ||| The SCGI headers as sent by the proxy server
  headers     : Headers

  ||| URI of the request
  uri         : URI

  ||| Content
  content     : ByteString

parameters {auto merr : MErr f}
           {auto has  : Has RequestErr es}

  export
  requestMethod : ByteString -> f es Method
  requestMethod bs =
    case toString bs of
      "GET"    => pure GET
      "HEAD"   => pure HEAD
      "POST"   => pure POST
      "PUT"    => pure PUT
      "DELETE" => pure DELETE
      "PATCH"  => pure PATCH
      s        => throw $ requestErrMsg "Unknown HTTP method: \{s}" badRequest400


  export
  requestURI : ByteString -> f es URI
  requestURI bs =
    case parseURI Virtual bs of
      Left  x => throw $ requestErrDetails x badRequest400
      Right u => pure u
