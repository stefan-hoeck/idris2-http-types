module HTTP.Request

import Data.ByteString
import HTTP.Header
import HTTP.Method
import HTTP.URI

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

  ||| The total size of the content in case of a POST request
  contentSize : Nat

  ||| Content
  content     : ByteString
