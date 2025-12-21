module HTTP.API.Server.Header

import HTTP.API.Server.Interface

%default total

decodeHeaders :
     (ps : List HeaderPart)
  -> {auto all : All Decode (HeaderTypes ps)}
  -> Request
  -> Either RequestErr (HList $ HeaderTypes ps)
decodeHeaders []             r = Right []
decodeHeaders (H n t s::hs) @{_::_} r =
  case lookupHeader n r.headers of
    Nothing => Left (requestErrMsg "Missing HTTP header: \{n}" s)
    Just bs => Prelude.do
      v  <- mapFst (decodeErr s) (decodeAs t bs)
      vs <- decodeHeaders hs r
      pure $ v::vs

public export
Serve ReqHeaders where
  Constraint h         = All Decode (HeaderTypes h.headers)
  InTypes  h           = HeaderTypes h.headers
  OutTypes _           = []
  outs     _           = []
  canHandle   _ r      = True
  fromRequest h r      = injectEither $ decodeHeaders h.headers r
  adjResponse _ [] req = pure
