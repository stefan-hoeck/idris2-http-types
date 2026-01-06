module HTTP.API.Client.Request

import HTTP.API
import Web.Internal.Types

%hide Types.Headers
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
  Bytes : (mediatype : String) -> ByteString -> RequestBody
  Str   : (mediatype : String) -> String -> RequestBody
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
