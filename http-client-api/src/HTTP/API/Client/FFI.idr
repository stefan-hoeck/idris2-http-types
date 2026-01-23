module HTTP.API.Client.FFI

import Data.Buffer
import Data.Buffer.Core
import Data.ByteString
import Data.Linear.Token
import HTTP.Method
import HTTP.URI
import Web.Internal.Types

%default total

%foreign "javascript:lambda:x=>x.length"
prim__buflen : Buffer -> Bits32

%foreign "browser:lambda:(x,a,b,w)=>x.append(a,b)"
prim__append : FormData -> String -> String -> PrimIO ()

%foreign "browser:lambda:(x,a,b,w)=>x.append(a,b)"
prim__appendBlob : FormData -> String -> Blob -> PrimIO ()

%foreign "browser:lambda:(x,a,b,w)=>x.append(a,b)"
prim__appendFile : FormData -> String -> File -> PrimIO ()

%foreign "browser:lambda:(x,a,b,w)=>x.append(a,new Blob(b))"
prim__appendBuffer : FormData -> String -> Buffer -> PrimIO ()

%foreign "browser:lambda:(x,w)=>{console.log(x.response); console.log(x.responseText); return (new Uint8Array(x.response));}"
prim__responseBytes : XMLHttpRequest -> PrimIO Buffer

%foreign "browser:lambda:(x,w)=>x.status"
prim__status : XMLHttpRequest -> PrimIO Bits16

%foreign "browser:lambda:(x,w)=>x.send()"
prim__send : XMLHttpRequest -> PrimIO ()

%foreign "browser:lambda:(x,s,w)=>x.send(s)"
prim__sendTxt : XMLHttpRequest -> String -> PrimIO ()

%foreign "browser:lambda:(x,s,w)=>x.send(s)"
prim__sendBuf : XMLHttpRequest -> Buffer -> PrimIO ()

%foreign "browser:lambda:(w)=> new FormData()"
prim__newFD : PrimIO FormData

%foreign "browser:lambda:(x,s,w)=>x.send(s)"
prim__sendFD : XMLHttpRequest -> FormData -> PrimIO ()

%foreign "browser:lambda:(w)=> new XMLHttpRequest()"
prim__request : PrimIO XMLHttpRequest

%foreign "browser:lambda:(x,w)=>x.readyState"
prim__readyState : XMLHttpRequest -> PrimIO Bits16

%foreign "browser:lambda:(x,v,w)=>{x.timeout = v}"
prim__setTimeout : XMLHttpRequest -> Bits32 -> PrimIO ()

%foreign "browser:lambda:(x,w)=>x.abort()"
prim__abort : XMLHttpRequest -> PrimIO ()

%foreign "browser:lambda:(x,me,url,w)=>x.open(me,url)"
prim__open : XMLHttpRequest -> String -> String -> PrimIO ()

%foreign "browser:lambda:(x,a,b,w)=>x.setRequestHeader(a,b)"
prim__setRequestHeader : XMLHttpRequest -> (h,v : String) -> PrimIO ()

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

export %inline
xmlhttpRequest : IO1 XMLHttpRequest
xmlhttpRequest = ffi prim__request

export %inline
status : XMLHttpRequest -> IO1 Bits16
status x = ffi $ prim__status x

export %inline
abort : XMLHttpRequest -> IO1 ()
abort x = ffi $ prim__abort x

export %inline
opn : XMLHttpRequest -> Method -> URI -> IO1 ()
opn x m u = ffi $ prim__open x (show m) "\{u}"

export %inline
send : XMLHttpRequest -> IO1 ()
send x = ffi $ prim__send x

export %inline
sendTxt : XMLHttpRequest -> String -> IO1 ()
sendTxt x s = ffi $ prim__sendTxt x s

export %inline
sendBuffer : XMLHttpRequest -> Buffer -> IO1 ()
sendBuffer x s = ffi $ prim__sendBuf x s

export %inline
sendFD : XMLHttpRequest -> FormData -> IO1 ()
sendFD x s = ffi $ prim__sendFD x s

export %inline
setRequestHeader : XMLHttpRequest -> String -> ByteString -> IO1 ()
setRequestHeader x n v = ffi $ prim__setRequestHeader x n (toString v)

export %inline
setRequestHeaderP : XMLHttpRequest -> (String, ByteString) -> IO1 ()
setRequestHeaderP x (n,v) = setRequestHeader x n v

export
responseBytes : XMLHttpRequest -> IO1 ByteString
responseBytes x t =
 let buf # t := ffi (prim__responseBytes x) t
  in unsafeByteString (cast $ prim__buflen buf) buf # t

export %inline
newFD : IO1 FormData
newFD = ffi prim__newFD

export %inline
appendTxt : FormData -> (name,value : String) -> IO1 ()
appendTxt fd n v = ffi $ prim__append fd n v

export %inline
appendBlob : FormData -> (name : String) -> Blob -> IO1 ()
appendBlob fd n v = ffi $ prim__appendBlob fd n v

export %inline
appendFile : FormData -> (name : String) -> File -> IO1 ()
appendFile fd n v = ffi $ prim__appendFile fd n v

export
appendBytes : FormData -> (name : String) -> ByteString -> IO1 ()
appendBytes x n bs t =
 let buf # t := ioToF1 (toBuffer bs) t
  in ffi (prim__appendBuffer x n buf) t
