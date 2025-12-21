module HTTP.API.Server.Interface

import public HTTP.API
import public HTTP.Prog
import public HTTP.Request
import public HTTP.Response

%default total

||| Interface for integrating a partial description of a
||| HTTP endpoint and using to extract variables from the
||| HTTP request and make adjustments to the server response.
public export
interface Serve (0 a : Type) where

  ||| Auto-implicit argument required by and passed to the
  ||| utility functions of this interface.
  0 Constraint : a -> Type

  ||| Types of values extracted from the HTTP request
  ||| in function `fromRequest`.
  0 InTypes    : a -> List Type

  ||| Types of values needed to adjust the HTTP response
  ||| in `adjResponse`.
  0 OutTypes   : a -> List Type

  ||| `TList` representation of `OutTypes`. This is required
  ||| to split the `OutTypes` of several endpoint descriptions and
  ||| pass each of them to the proper description.
  outs         : (v : a) -> TList (OutTypes v)

  ||| Returns `True`, if this endpoint description can process
  ||| the current HTTP request.
  |||
  ||| In general, a HTTP server consists of many endpoints, and
  ||| the first, for which `canHandle` returns `True`, will
  ||| get to process the request. If it then fails to do so
  ||| (by failing with a `RequestErr` in `fromRequest` or `adjResposnse`,
  ||| the request is considered to be invalid and no other endpoint
  ||| handler is tried. Instead, a proper error response is sent to
  ||| the client.
  canHandle    : (v : a) -> (con : Constraint v) => Request -> Bool

  ||| Tries to analyze the current HTTP request and extract
  ||| a heterogeneous list of `InTypes` from it.
  fromRequest  :
       (v : a)
    -> {auto con : Constraint v}
    -> Request
    -> Handler (HList $ InTypes v)

  ||| Takes the `OutTypes` and uses them to adjust the HTTP response.
  adjResponse  :
       (v : a)
    -> {auto con : Constraint v}
    -> HList (OutTypes v)
    -> Request
    -> Response
    -> Handler Response

--------------------------------------------------------------------------------
-- Type-level Utilities
--------------------------------------------------------------------------------

||| Computes the list of constraints required to process a
||| single endpoint description.
public export
0 Constraints : All Serve ts => (endpoint : HList ts) -> List Type
Constraints @{[]}   []      = []
Constraints @{_::_} (v::vs) = Constraint v :: Constraints vs

||| Computes the list values extracted from the HTTP request
||| by a single endpoint description.
public export
0 AllInTypes : All Serve ts => (endpoint : HList ts) -> List Type
AllInTypes @{[]}   []      = []
AllInTypes @{_::_} (v::vs) = InTypes v ++ AllInTypes vs

||| Computes the list values used to adjust the HTTP response
||| by a single endpoint description.
public export
0 AllOutTypes : All Serve ts => (endpoint : HList ts) -> List Type
AllOutTypes @{[]}   []      = []
AllOutTypes @{_::_} (v::vs) = OutTypes v ++ AllOutTypes vs

||| Computes a function type from a list of argument types and a result type.
|||
||| This is used to compute the function type of handler of a single
||| server endpoint.
public export
0 Fun : List Type -> Type -> Type
Fun []        r = r
Fun (t :: ts) r = t -> Fun ts r

||| Computes the result type the handler of a server endpoint.
|||
||| For convenience in downstream code, we only return a heterogeneous list
||| in case several values are required to make adjustments to the HTTP
||| response. In case of no value or a single value being required, we return
||| `Unit` or the a single value, respectively.
public export
0 ResultType : List Type -> Type
ResultType []  = ()
ResultType [t] = t
ResultType ts  = HList ts

||| Computes a `TList` representation of the outtypes of an entpoint.
public export
allOuts : All Serve ts => (endpoint : HList ts) -> TList (AllOutTypes endpoint)
allOuts @{[]}   []      = []
allOuts @{_::_} (v::vs) = outs v ++ allOuts vs

||| Function type for the handler of a single endpoint of a HTTP server.
public export
0 EndpointHandler : All Serve ts => (endpoint : HList ts) -> Type
EndpointHandler vs =
  Fun (AllInTypes vs) (Handler (ResultType $ AllOutTypes vs))

--------------------------------------------------------------------------------
-- Server Implementation
--------------------------------------------------------------------------------

applyHList : HList ts -> Fun ts (Handler o) -> Handler o
applyHList []        r = r
applyHList (v :: vs) f = applyHList vs (f v)

-- converts a `ResultType` to a heterogeneous list
wrapResult : TList ts -> ResultType ts -> HList ts
wrapResult []        r = []
wrapResult [t]       r = [r]
wrapResult (_::_::_) r = r

||| Returns `True` if the given endpoint can serve the current
||| HTTP request
export
canServe :
     {auto all : All Serve ts}
  -> (endpoint : HList ts)
  -> {auto con : HList (Constraints endpoint)}
  -> Request -> Bool
canServe @{[]}   []      @{[]}   req = True
canServe @{_::_} (v::vs) @{_::_} req = canHandle v req && canServe vs req

-- extracts all required values from the HTTP request at a given
-- endpoint
getIns :
     {auto all : All Serve ts}
  -> (endpoint : HList ts)
  -> {auto con : HList (Constraints endpoint)}
  -> Request
  -> Handler (HList $ AllInTypes endpoint)
getIns @{[]}   []      @{[]}   req = pure []
getIns @{_::_} (v::vs) @{_::_} req = Prelude.do
  rs  <- fromRequest v req
  rem <- getIns vs req
  pure (rs ++ rem)

-- uses the values generated by the endpoint handler to make
-- adjustments to the HTTP response.
putOuts :
     {auto all : All Serve ts}
  -> (endpoint : HList ts)
  -> {auto con : HList (Constraints endpoint)}
  -> HList (AllOutTypes endpoint)
  -> Request
  -> Response
  -> Handler Response
putOuts @{[]}   []      [] @{[]}   req resp = pure resp
putOuts @{_::_} (x::xs) vs @{_::_} req resp = Prelude.do
  let (ts,rem) := splitHList (outs x) vs
  r2 <- adjResponse x ts req resp
  putOuts xs rem req r2

||| Processes a HTTP request at the given HTTP endpoint.
|||
||| This proceeds in three steps:
|||
||| 1. The necessary values are extracted from the request
|||    by invoking all `fromRequest` functions and concatenating
|||    the results.
|||
||| 2. The extracted values are passed to the endpoint handler
|||    (of type `EndpointHandler endpoint`), which produces the values
|||    required to make adjustments to the HTTP response.
|||
||| 3. The values from (2) are split up and passed to the individual
|||    `adjResponse` runctions, which adjust the (initially empty)
|||    HTTP response accordingly.
export
serveEndpoint :
     {auto all : All Serve ts}
  -> (endpoint : HList ts)
  -> {auto con : HList (Constraints endpoint)}
  -> EndpointHandler endpoint
  -> Request
  -> Handler Response
serveEndpoint endpoint f req = Prelude.do
  ins  <- getIns endpoint req
  outs <- applyHList ins f
  putOuts endpoint (wrapResult (allOuts endpoint) outs) req empty
