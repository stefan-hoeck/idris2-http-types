module HTTP.API.Client.Path

import HTTP.API.Client.Interface

%default total

export
recTypes : (ps : List Part) -> TList (PartsTypes ps)
recTypes []                = []
recTypes (PScheme _ :: xs) = recTypes xs
recTypes (PAuth _   :: xs) = recTypes xs
recTypes (PStr _    :: xs) = recTypes xs
recTypes (PTill _   :: xs) = recTypes xs
recTypes (Capture t :: xs) = t :: recTypes xs

reqPath :
     (ps : List Part)
  -> All EncodeMany (PartsTypes ps)
  -> HList (PartsTypes ps)
  -> List ByteString
reqPath []        _  _  = []
reqPath (PScheme _ :: ps) es      vs      = reqPath ps es vs
reqPath (PAuth _   :: ps) es      vs      = reqPath ps es vs
reqPath (PStr s    :: ps) es      vs      = fromString s :: reqPath ps es vs
reqPath (PTill s   :: ps) es      vs      = fromString s :: reqPath ps es vs
reqPath (Capture _ :: ps) (e::es) (v::vs) = encodeMany v ++ reqPath ps es vs

adjAuth : List Part -> HTTPRequest -> HTTPRequest
adjAuth []             r = r
adjAuth (PAuth s :: _) r = adjURI {authority := Just $ cast s} r
adjAuth (_ :: xs)      r = adjAuth xs r

adjScheme : List Part -> HTTPRequest -> HTTPRequest
adjScheme []               r = r
adjScheme (PScheme s :: _) r = adjURI {scheme := Just $ cast s} r
adjScheme (_ :: xs)        r = adjScheme xs r

public export
Receive ReqPath where
  RecConstraint p = All EncodeMany (PartsTypes p.parts)
  RecTypes p = PartsTypes p.parts
  recs p = recTypes p.parts
  adjRequest (Path ps) vs r =
    let pth := reqPath ps con vs
     in adjURI {path := pth} . adjScheme ps $ adjAuth ps r

public export
GetResponse ReqPath where
  RespEncodings _ = []
  RespTypes _ = []
