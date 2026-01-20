module HTTP.API.Client.Request

import Data.Linear.Traverse1
import HTTP.API
import HTTP.API.Client.FFI
import Syntax.T1
import Web.Async
import Web.Internal.Types

%hide Types.Headers
%hide JS.ByteString.ByteString
%default total

||| Part of a `FormData` object
public export
data FDPart : Type where
  FDBlob   : Blob -> FDPart
  FDBytes  : ByteString -> FDPart
  FDFile   : File -> FDPart
  FDString : String -> FDPart

public export
data RequestBody : Type where
  None  : RequestBody
  Bytes : MediaType -> ByteString -> RequestBody
  Str   : MediaType -> String -> RequestBody
  FD    : List (String,FDPart) -> RequestBody

public export
record HTTPRequest where
  constructor R
  method  : Method
  uri     : URI
  headers : Headers
  body    : RequestBody

export %inline
adjURI : (URI -> URI) -> HTTPRequest -> HTTPRequest
adjURI f = {uri $= f}

export
emptyRequest : HTTPRequest
emptyRequest =
  R GET (MkURI Nothing Nothing False [] [] Nothing) emptyHeaders None

--------------------------------------------------------------------------------
-- Interface
--------------------------------------------------------------------------------

public export
interface RequestEncode (0 from,to : Type) where
  reqEncodeAs  : from -> to
  toBody       : to -> RequestBody

export %inline
(e : EncodeVia f t) => RequestEncode f t where
  reqEncodeAs  = encodeAs
  toBody       = Bytes (mediaType @{e}) . fastConcat . toBytes @{e}

--------------------------------------------------------------------------------
-- Sending Requests
--------------------------------------------------------------------------------

||| HTTP Errors
public export
data HTTPError : Type where
  Timeout      : HTTPError
  NetworkError : HTTPError
  ReqError     : RequestErr -> HTTPError

setFD : FormData -> (String,FDPart) -> IO1 ()
setFD fd (nm,FDBlob b)   t = appendBlob fd nm b t
setFD fd (nm,FDBytes b)  t = appendBytes fd nm b t
setFD fd (nm,FDFile f)   t = appendFile fd nm f t
setFD fd (nm,FDString s) t = appendTxt fd nm s t

sendBody : XMLHttpRequest -> RequestBody -> IO1 ()
sendBody x None         t = send x t
sendBody x (Bytes mt b) t =
 let _   # t := setRequestHeader x Content_Type (encodeMediaType mt) t
     buf # t := ioToF1 (toBuffer b) t
  in sendBuffer x buf t
sendBody x (Str mt s)  t =
 let _   # t := setRequestHeader x Content_Type (encodeMediaType mt) t
  in sendTxt x s t
sendBody x (FD xs)     t =
 let _   # t := setRequestHeader x Content_Type "multipart/form-data" t
     fd  # t := newFD t
     _   # t := for1_ xs (setFD fd) t
  in sendFD x fd t

parameters {auto has : Has HTTPError es}
           {auto dec : DecodeVia f t}
           (cb       : Result es t -> IO1 ())

  onerror : HTTPError -> Event -> IO1 ()
  onerror x _ t = cb (Left $ inject x) t

  onsuccess : XMLHttpRequest -> IO1 ()

  onload : XMLHttpRequest -> Event -> IO1 ()
  onload x ev = T1.do
    st   <- status x
    case st >= 200 && st < 300 of
      False => ?decodeRequestErr
      True  => onsuccess x

  export
  send1 : HTTPRequest -> IO1 (IO1 ())
  send1 r = T1.do
    x <- xmlhttpRequest
    addEventListener (up x) "error" $ onerror NetworkError
    addEventListener (up x) "load" $ onload x
    addEventListener (up x) "timeout" $ onerror Timeout
    opn x r.method r.uri

    for1_ (kvList r.headers) $ setRequestHeaderP x
    sendBody x r.body

    pure (abort x)
