module HTTP.API.Server.Content

import HTTP.API.Server.Interface

%default total

decodeBody : All (`DecodeVia` t) ts -> Request -> Either RequestErr (HList [t])
decodeBody []        r = Left $ requestErr unsupportedMediaType415
decodeBody (d :: ds) r =
  case contentType r.headers of
    Nothing         => decodeBody ds r
    Just (CT mt ps) => case mt == mediaType @{d} of
      False => decodeBody ds r
      True  =>
        bimap
          (decodeErr badRequest400)
          (\x => [x])
          (decodeVia @{d} ps r.content)

public export
Serve ReqContent where
  Constraint b         = All (`DecodeVia` b.result) b.formats
  InTypes  b           = [b.result]
  OutTypes _           = []
  outs     _           = %search
  canHandle   _ r      = True
  fromRequest _ r      = injectEither $ decodeBody con r
  adjResponse _ [] req = pure
