module HTTP.API.Server.Method

import HTTP.API.Server.Interface

%default total

checkResponseTypes : All (EncodeVia t) ts -> Request -> Handler ()
checkResponseTypes a r =
  case any (acceptsMedia r.headers) (forget $ mapProperty (\x => mediaType @{x}) a) of
    True  => pure ()
    False => throw $ requestErr unsupportedMediaType415

public export
Serve ReqMethod where
  InTypes m = []

  OutTypes m = MethodResults m

  Constraint (M _ _ _ Nothing0)   = ()
  Constraint (M _ _ fs $ Just0 t) = All (EncodeVia t) fs

  outs (M _ _ _ Nothing0)  = []
  outs (M _ _ _ $ Just0 t) = [t]

  canHandle (M m _ _ _) r = m == r.method
  fromRequest m r = pure []
  adjResponse (M _ _ _ Nothing0)  _   req resp = pure resp
  adjResponse (M _ s _ $ Just0 _) [v] req resp = do
    checkResponseTypes con req
    pure $ encodeBody s v req.headers con resp
