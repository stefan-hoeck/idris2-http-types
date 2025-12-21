module HTTP.API.Server.Query

import Data.List
import HTTP.API.Server.Interface

%default total

convertQ :
     (fs : List QField)
  -> All Decode (QueryConstraintTypes fs)
  -> Queries
  -> Either DecodeErr (HList (QueryTypes fs))
convertQ []               []        qs = Right []
convertQ ((n ?? t) :: xs) (y :: ys) qs = Prelude.do
  let Just (QVal bs) := lookup n qs
        | Nothing     => Left (Msg "Missing query parameter: '\{n}'")
        | Just QEmpty => Left (Msg "Missing query value: '\{n}'")
  v  <- decodeAs t bs
  vs <- convertQ xs ys qs
  Right $ v::vs
convertQ (QBool n :: xs) ys qs = Prelude.do
  vs <- convertQ xs ys qs
  Right $ isJust (lookup n qs) :: vs

public export
Serve ReqQuery where
  InTypes q               = QueryTypes q.fields
  OutTypes _              = []
  Constraint q            = All Decode (QueryConstraintTypes q.fields)
  outs q                  = []
  canHandle _ r           = True
  adjResponse _ _ _       = pure
  fromRequest q r         =
    either
      (throw . decodeErr badRequest400)
      pure
      (convertQ q.fields con r.uri.queries)

