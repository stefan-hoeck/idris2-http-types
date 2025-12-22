module HTTP.API.Env

import System.Clock

%default total

||| Description of a form of "environment" variable that needs to
||| be generated and available when processing a request.
public export
record ReqEnv where
  constructor Env
  0 type : Type

||| Time, at which the request is being processed.
public export
record ReqTime where
  constructor RT
  time : Clock UTC
