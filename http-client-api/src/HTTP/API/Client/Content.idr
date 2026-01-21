module HTTP.API.Client.Content

import HTTP.API.Client.Interface

%default total

adj : Any (RequestEncode t) ts -> t -> HTTPRequest -> HTTPRequest
adj (Here x)  v = {body := toBody @{x} (reqEncodeAs v)}
adj (There x) v = adj x v

public export
Receive ReqContent where
  RecConstraint c = Any (RequestEncode c.result) c.formats
  RecTypes c = [c.result]
  recs     c = [c.result]
  adjRequest c [v] = adj con v

public export
GetResponse ReqContent where
  RespEncodings _ = []
  RespTypes _ = []
