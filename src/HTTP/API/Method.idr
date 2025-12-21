module HTTP.API.Method

import public Data.Maybe0
import HTTP.Method
import HTTP.Status

||| Data type describing the method (verb), response status code,
||| as well as encoding formats and content type (if any) of a HTTP
||| endpoint.
|||
||| Please note that `format` and `result` refer to the format and
||| type of the content in the server's *response* body.
|||
||| See `HTTP.API.Content` for a way to describe format and content type
||| in the HTTP *request* body.
public export
record ReqMethod where
  constructor M
  method    : Method
  status    : Status
  0 formats : List Type
  result  : Maybe0 Type

public export
0 MethodResults : ReqMethod -> List Type
MethodResults (M _ _ _ $ Just0 t)  = [t]
MethodResults (M _ _ _ $ Nothing0) = []

public export
Get : (0 formats : List Type) -> (0 val : Type) -> ReqMethod
Get fs v = M GET ok200 fs (Just0 v)

public export
Post : (0 formats : List Type) -> (0 val : Type) -> ReqMethod
Post fs v = M POST ok200 fs (Just0 v)

public export
PostCreated : (0 formats : List Type) -> (0 val : Type) -> ReqMethod
PostCreated fs v = M POST created201 fs (Just0 v)

public export
Put : (0 formats : List Type) -> (0 val : Type) -> ReqMethod
Put fs v = M PUT ok200 fs (Just0 v)

public export
PutCreated : (0 formats : List Type) -> (0 val : Type) -> ReqMethod
PutCreated fs v = M PUT created201 fs (Just0 v)

public export
Patch : (0 formats : List Type) -> (0 val : Type) -> ReqMethod
Patch fs v = M PATCH ok200 fs (Just0 v)

public export
Head : ReqMethod
Head = M HEAD noContent204 [] Nothing0

public export
Delete : ReqMethod
Delete = M DELETE noContent204 [] Nothing0

public export
Post' : ReqMethod
Post' = M POST noContent204 [] Nothing0

public export
Put' : ReqMethod
Put' = M PUT noContent204 [] Nothing0

public export
Patch' : ReqMethod
Patch' = M PATCH noContent204 [] Nothing0
