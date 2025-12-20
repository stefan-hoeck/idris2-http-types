module HTTP.Response

import HTTP.API
import JSON.Simple

%default total

--------------------------------------------------------------------------------
--          Response
--------------------------------------------------------------------------------

||| HTTP Response
|||
||| Currently, we include the content / body as a whole. This might
||| be changed to a stream of data if we ever decide to stream
||| large amounts of content.
public export
record Response where
  constructor RP
  headers : Headers
  content : List ByteString

export
empty : Response
empty = RP emptyHeaders []

export
addHeader :  ByteString -> ByteString  -> Response -> Response
addHeader x y = {headers $= insertHeader x y}

export
setStatus : Status -> Response -> Response
setStatus s = addHeader "Status" (cast s)

crlf : ByteString
crlf = "\r\n"

export
responseBytes : Response -> List ByteString
responseBytes (RP hs bs) =
  case kvList hs of
    [] => crlf :: crlf :: bs
    ps => (ps >>= \(h,v) => [h,":",v,crlf]) ++ crlf :: bs

export
setContentType : EncodeVia f t -> Response -> Response
setContentType e = addHeader "Content-Type" (fromString $ mediaType @{e})

export
encodeBody :
     Status
  -> t
  -> Headers
  -> All (EncodeVia t) ts
  -> Response
  -> Response
encodeBody s v hs []        rs = setStatus s rs
encodeBody s v hs (e :: es) rs =
  case acceptsMedia hs (mediaType @{e}) of
    False => encodeBody s v hs es rs
    True  => {content := encodeVia v e} rs |> setContentType e |> setStatus s

--------------------------------------------------------------------------------
--          Common Responses
--------------------------------------------------------------------------------

encErr : All (EncodeVia RequestErr) [JSON, Text]
encErr = %search

export
fromError : Maybe URI -> Headers -> RequestErr -> Response
fromError mu hs re@(RE st err _ _ _) =
 let u := maybe "" interpolate mu
  in encodeBody (MkStatus st err) ({path := u} re) hs encErr empty

export
fromStatus : URI -> Headers -> Status -> Response
fromStatus u hs = fromError (Just u) hs . requestErr

export
notFound : URI -> Headers -> Response
notFound u hs = fromStatus u hs notFound404

export
forbidden : URI -> Headers -> Response
forbidden u hs = fromStatus u hs forbidden403
