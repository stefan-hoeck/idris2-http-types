module HTTP.API.Client.Request

import HTTP.API

%default total

public export
record Request where
  constructor R
  uri     : URI
  headers : Headers
