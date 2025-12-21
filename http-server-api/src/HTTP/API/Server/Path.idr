module HTTP.API.Server.Path

import HTTP.API.Server.Interface

%default total

canHandlePath :
     (ps : List Part)
  -> All DecodeMany (PartsTypes ps)
  -> List ByteString
  -> Bool
canHandlePath [] [] [] = True
canHandlePath (PStr s :: ys) xs (p :: ps) =
  s == toString p && canHandlePath ys xs ps
canHandlePath (Capture t :: ys) (x::xs) ps =
  case simulateDecode @{x} ps of
    Just ps2 => canHandlePath ys xs ps2
    Nothing  => False
canHandlePath _ _ _ = False

convertRequest :
     (ps : List Part)
  -> All DecodeMany (PartsTypes ps)
  -> List ByteString
  -> Either DecodeErr (HList $ PartsTypes ps)
convertRequest [] []  [] = Right []
convertRequest (PStr s    :: ys) as (b::bs) = convertRequest ys as bs
convertRequest (Capture t :: ys) (a::as) bs = Prelude.do
  (bs2,v) <- decodeMany @{a} bs
  vs      <- convertRequest ys as bs2
  pure $ v::vs
convertRequest _ _ _ = Left (Msg "Unexpected end of URI path")

public export
Serve ReqPath where
  InTypes    m = PartsTypes m.parts
  OutTypes   m = []
  Constraint m = All DecodeMany (PartsTypes m.parts)
  outs       _ = %search
  canHandle m r = canHandlePath m.parts con r.uri.path
  adjResponse m _ _ = pure
  fromRequest m r =
    either
      (throw . decodeErr badRequest400)
      pure
      (convertRequest m.parts con r.uri.path)

