module HTTP.API.Client.Method

import HTTP.API.Client.Interface

%default total

public export
Receive ReqMethod where
  RecConstraint _ = ()
  RecTypes _ = []
  recs     _ = []
  adjRequest m _ r = {method := m.method} r
