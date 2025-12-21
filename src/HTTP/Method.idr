module HTTP.Method

import Derive.Prelude

%default total
%language ElabReflection

||| An enumeration of HTTP request methods.
public export
data Method : Type where

  ||| Requests a representation of the specified resource.
  ||| GET requests should not contain a request body.
  GET    : Method

  ||| Requests the metadata of a resource. This is typically sent
  ||| instead of a `GET` request, for instance to determine the
  ||| `Content-Size` of a resource.
  HEAD   : Method

  ||| Submits some data to the specified resource, often resulting
  ||| in a change of the server state.
  POST   : Method

  ||| Creates a new resource or replaces an existing resource with
  ||| the request content. Unlike `POST`, `PUT` requests are supposed
  ||| to be idempotent.
  PUT    : Method

  ||| Asks the HTTP server to delete a specified resource.
  DELETE : Method

  ||| Applies a partial modification to a resource.
  PATCH : Method

%runElab derive "Method" [Show,Eq,Ord]
