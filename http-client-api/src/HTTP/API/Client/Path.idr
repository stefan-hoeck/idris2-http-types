module HTTP.API.Client.Path

import HTTP.API.Client.Interface

%default total

export
recTypes : (ps : List Part) -> TList (PartsTypes ps)
recTypes []                = []
recTypes (PStr _    :: xs) = recTypes xs
recTypes (PTill _   :: xs) = recTypes xs
recTypes (Capture t :: xs) = t :: recTypes xs

reqPath :
     (ps : List Part)
  -> All EncodeMany (PartsTypes ps)
  -> HList (PartsTypes ps)
  -> List ByteString
reqPath []        _  _  = []
reqPath (PStr s    :: ps) es      vs      = fromString s :: reqPath ps es vs
reqPath (PTill s   :: ps) es      vs      = fromString s :: reqPath ps es vs
reqPath (Capture _ :: ps) (e::es) (v::vs) = encodeMany v ++ reqPath ps es vs

public export
Receive ReqPath where
  RecConstraint p = All EncodeMany (PartsTypes p.parts)
  RecTypes p = PartsTypes p.parts
  recs p = recTypes p.parts
  adjRequest p vs r =
    let pth := reqPath p.parts con vs
     in adjURI {path := pth} r
