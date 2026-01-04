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
  Bytes : ByteString -> RequestBody
  Str   : String -> RequestBody
  FD    : List (String,FDPart) -> RequestBody

public export
record HTTPRequest where
  constructor R
  method  : Method
  uri     : URI
  headers : Headers
  body    : RequestBody

export
emptyRequest : HTTPRequest
emptyRequest =
  R GET (MkURI Nothing Nothing False [] [] Nothing) emptyHeaders None
