module HTTP.API.Content

%default total

||| Data type for describing the format(s) and type of some
||| data stored in a HTTP request's body.
public export
record ReqContent where
  constructor Content
  0 formats : List Type
  0 result  : Type

