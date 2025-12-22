module HTTP.API.Server.Env

import HTTP.API.Server.Interface
import System.Clock

%default total

public export
interface LoadEnv (0 a : Type) where
  loadEnv : HTTPProg [RequestErr] a

export %inline
LoadEnv ReqTime where
  loadEnv = RT <$> liftIO (clockTime UTC)

public export
Serve ReqEnv where
  Constraint b         = LoadEnv b.type
  InTypes  b           = [b.type]
  OutTypes _           = []
  outs     _           = %search
  canHandle   _ r      = True
  fromRequest _ r      = map (\x => [x]) loadEnv
  adjResponse _ [] req = pure
