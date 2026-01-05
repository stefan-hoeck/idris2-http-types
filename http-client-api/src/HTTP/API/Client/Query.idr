module HTTP.API.Client.Query

import HTTP.API.Client.Interface

%default total

export
queryTypes : (fs : List QField) -> TList (QueryTypes fs)
queryTypes []               = []
queryTypes ((_ ?? t) :: xs) = t :: queryTypes xs
queryTypes (QBool _ :: xs)  = Bool :: queryTypes xs

qus :
     (fs : List QField)
  -> All Encode (QueryConstraintTypes fs)
  -> HList (QueryTypes fs)
  -> Queries
qus []             _       _       = []
qus ((n ?? t)::fs) (e::es) (v::vs) = (n,QVal $ encode v)::qus fs es vs
qus (QBool n ::fs) es      (v::vs) =
  if v then (n,QEmpty)::qus fs es vs else qus fs es vs

public export
Receive ReqQuery where
  RecConstraint q = All Encode (QueryConstraintTypes q.fields)
  RecTypes q = QueryTypes q.fields
  recs q = queryTypes q.fields
  adjRequest q vs r = adjURI {queries := qus q.fields con vs} r

