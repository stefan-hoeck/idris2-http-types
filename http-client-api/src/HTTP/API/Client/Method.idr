module HTTP.API.Client.Method

import HTTP.API.Client.Interface

%default total

public export
Receive ReqMethod where
  RecConstraint _ = ()
  RecTypes _ = []
  recs     _ = []
  adjRequest m _ r = {method := m.method} r

public export
GetResponse ReqMethod where
  RespEncodings m = m.formats
  RespTypes (M _ _ _ Nothing0) = []
  RespTypes (M _ _ _ $ Just0 t) = [t]
