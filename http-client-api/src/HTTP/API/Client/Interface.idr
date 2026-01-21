module HTTP.API.Client.Interface

import public HTTP.API
import public HTTP.API.Client.Request

%default total

public export
interface Receive (0 a : Type) where
  ||| Auto-implicit argument required by and passed to the
  ||| utility functions of this interface.
  0 RecConstraint : a -> Type

  ||| Types of values extracted from the HTTP request
  ||| in function `fromRequest`.
  0 RecTypes : a -> List Type

  ||| `TList` representation of `RecTypes`. This is required
  ||| to split the `RecTypes` of several endpoint descriptions and
  ||| pass each of them to the proper description.
  recs       : (v : a) -> TList (RecTypes v)

  adjRequest :
       (v : a)
    -> {auto con : RecConstraint v}
    -> HList (RecTypes v)
    -> HTTPRequest
    -> HTTPRequest

public export
interface GetResponse (0 a : Type) where
  0 RespEncodings : a -> List Type
  0 RespTypes : a -> List Type

--------------------------------------------------------------------------------
-- Type-level Utilities
--------------------------------------------------------------------------------

public export
0 AllRespTypes : All GetResponse ts => (endpoint : HList ts) -> List Type
AllRespTypes @{[]}   []      = []
AllRespTypes @{_::_} (v::vs) = RespTypes v ++ AllRespTypes vs

public export
0 AllRespEncodings : All GetResponse ts => (endpoint : HList ts) -> List Type
AllRespEncodings @{[]}   []      = []
AllRespEncodings @{_::_} (v::vs) = RespEncodings v ++ AllRespEncodings vs

||| Computes the list of type of values used to adjust the HTTP request
||| by a single endpoint description.
public export
0 AllRecTypes : All Receive ts => (endpoint : HList ts) -> List Type
AllRecTypes @{[]}   []      = []
AllRecTypes @{_::_} (v::vs) = RecTypes v ++ AllRecTypes vs

||| Computes the list of type of constraints used to adjust the HTTP request
||| by a single endpoint description.
public export
0 AllRecConstraints : All Receive ts => (endpoint : HList ts) -> List Type
AllRecConstraints @{[]}   []      = []
AllRecConstraints @{_::_} (v::vs) = RecConstraint v :: AllRecConstraints vs

export
endpointRequest :
     (endpoint  : HList ts)
  -> {auto all  : All Receive ts}
  -> {auto cons : HList (AllRecConstraints endpoint)}
  -> HList (AllRecTypes endpoint)
  -> HTTPRequest
  -> HTTPRequest
endpointRequest []                      _  r = r
endpointRequest (v::vs) @{_::_} @{_::_} hl r =
 let (ts,rem) := splitHList (recs v) hl
  in endpointRequest vs rem (adjRequest v ts r)
