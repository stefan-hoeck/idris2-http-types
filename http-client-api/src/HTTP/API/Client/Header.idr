module HTTP.API.Client.Header

import HTTP.API.Client.Interface

%default total

export
headerTypes : (hs : List HeaderPart) -> TList (HeaderTypes hs)
headerTypes []              = []
headerTypes (H _ t _ :: xs) = t::headerTypes xs

adj :
     (hs : List HeaderPart)
  -> All Encode (HeaderTypes hs)
  -> HList (HeaderTypes hs)
  -> HTTPRequest
  -> HTTPRequest
adj []              _       _       r = r
adj (H n _ _ :: ps) (e::es) (v::vs) r =
  adj ps es vs $ {headers $= insertHeader n (encode v)} r

public export
Receive ReqHeaders where
  RecConstraint h = All Encode (HeaderTypes h.headers)
  RecTypes h = HeaderTypes h.headers
  recs h = headerTypes h.headers
  adjRequest h vs r = adj h.headers con vs r
