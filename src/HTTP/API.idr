module HTTP.API

import public Data.Buffer
import public Data.ByteString
import public Data.List.Quantifiers
import public Data.Maybe0

import public HTTP.API.Content
import public HTTP.API.Endpoints
import public HTTP.API.Decode
import public HTTP.API.Encode
import public HTTP.API.Env
import public HTTP.API.Header
import public HTTP.API.Method
import public HTTP.API.Path
import public HTTP.API.Query
import public HTTP.API.TList

import public HTTP.Header
import public HTTP.Method
import public HTTP.RequestErr
import public HTTP.Status
import public HTTP.URI

%default total

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

export
applyHList : HList ts -> Fun ts r -> r
applyHList []        r = r
applyHList (v :: vs) f = applyHList vs (f v)

||| converts a `ResultType` to a heterogeneous list
export
wrapResult : TList ts -> ResultType ts -> HList ts
wrapResult []        r = []
wrapResult [t]       r = [r]
wrapResult (_::_::_) r = r
