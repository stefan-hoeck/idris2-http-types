module HTTP.API.Client

import Data.Linear.Traverse1
import HTTP.API.Client.FFI
import JSON.Simple
import Syntax.T1
import Web.Async
import Web.Internal.Types

import public HTTP.API.Client.Content
import public HTTP.API.Client.Header
import public HTTP.API.Client.Interface
import public HTTP.API.Client.Method
import public HTTP.API.Client.Path
import public HTTP.API.Client.Query
import public HTTP.API.Client.Request

%default total

||| HTTP Errors
public export
data HTTPError : Type where
  Timeout      : HTTPError
  NetworkError : HTTPError
  DecError     : DecodeErr -> HTTPError
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

data Decoder : Type -> Type where
  NoDec : Decoder ()
  Dec   : {0 f : Type} -> DecodeVia f t -> Decoder t

adjHeader : Decoder t -> XMLHttpRequest -> IO1 ()
adjHeader NoDec   x t = () # t
adjHeader (Dec d) x t =
  setRequestHeader x Accept (encodeMediaType $ mediaType @{d}) t

parameters {auto has : Has HTTPError es}
           (dec      : Decoder t)
           (cb       : Result es t -> IO1 ())

  onerror : HTTPError -> Event -> IO1 ()
  onerror x _ = cb (Left $ inject x)

  onsuccess : XMLHttpRequest -> IO1 ()
  onsuccess x =
    case dec of
      NoDec => cb (Right ())
      Dec d => T1.do
        bs <- responseBytes x
        cb (mapFst (inject . DecError) $ decodeVia @{d} bs)

  onload : XMLHttpRequest -> Event -> IO1 ()
  onload x ev = T1.do
    st <- status x
    case st >= 200 && st < 300 of
      False => T1.do
        bs <- responseBytes x
        let res := decodeVia {from = JSON, to = RequestErr} bs
        cb (Left $ either (inject . DecError) (inject . ReqError) res)
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
    adjHeader dec x
    sendBody x r.body

    pure (abort x)

parameters {auto has  : Has HTTPError es}
           (endpoint  : HList ts)
           {auto all  : All Receive ts}
           {auto cons : HList (AllRecConstraints endpoint)}
           (args      : HList (AllRecTypes endpoint))

  export
  sendEndpoint : JS es ()
  sendEndpoint =
    primAsync $ \cb =>
      send1 NoDec cb (endpointRequest endpoint args emptyRequest)

  export
  requestEndpoint : (0 f,t : Type) -> (dec : DecodeVia f t) => JS es t
  requestEndpoint f t =
    primAsync $ \cb =>
      send1 (Dec dec) cb (endpointRequest endpoint args emptyRequest)

  export %inline
  requestJSONEndpoint : (0 t : Type) -> DecodeVia JSON t => JS es t
  requestJSONEndpoint = requestEndpoint JSON
