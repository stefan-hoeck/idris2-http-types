module HTTP.API.Client.FFI

import Data.Buffer
import Data.Buffer.Core
import Data.ByteString
import Data.Linear.Token
import Web.Internal.Types

%default total

%foreign "javascript:lambda:x=>x.length"
prim__buflen : Buffer -> Bits32

%foreign "browser:lambda:(x,a,b,w)=>x.append(a,b)"
prim__append : FormData -> String -> String -> PrimIO ()

%foreign "browser:lambda:(x,a,b,w)=>x.append(a,b)"
prim__appendBlob : FormData -> String -> Blob -> PrimIO ()

%foreign "browser:lambda:(x,w)=>new Uint8Array(x.response)"
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
xmlhttpRequest : HasIO io => io XMLHttpRequest
xmlhttpRequest = primIO prim__request

export %inline
send : HasIO io => XMLHttpRequest -> io ()
send x = primIO $ prim__send x

export %inline
sendTxt : HasIO io => XMLHttpRequest -> String -> io ()
sendTxt x s = primIO $ prim__sendTxt x s

export %inline
sendBuffer : HasIO io => XMLHttpRequest -> Buffer -> io ()
sendBuffer x s = primIO $ prim__sendBuf x s

export %inline
sendFD : HasIO io => XMLHttpRequest -> FormData -> io ()
sendFD x s = primIO $ prim__sendFD x s

export
responseBytes : XMLHttpRequest -> IO ByteString
responseBytes x = Prelude.do
  buf <- primIO $ prim__responseBytes x
  pure $ unsafeByteString (cast $ prim__buflen buf) buf
