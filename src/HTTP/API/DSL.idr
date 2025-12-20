module HTTP.API.DSL

import public Data.Buffer
import public Data.ByteString
import public Data.List.Quantifiers
import public Data.Maybe0
import public HTTP.API.Decode
import public HTTP.API.Encode
import public HTTP.Method
import public HTTP.Status

%default total

--------------------------------------------------------------------------------
-- Request Body
--------------------------------------------------------------------------------

public export
record ReqBody where
  constructor Body
  0 formats : List Type
  0 result  : Type

--------------------------------------------------------------------------------
-- Method
--------------------------------------------------------------------------------

public export
record ReqMethod where
  constructor M
  method    : Method
  status    : Status
  0 formats : List Type
  0 result  : Maybe0 Type

public export
0 MethodResult : ReqMethod -> Type
MethodResult (M _ _ _ $ Just0 t)  = t
MethodResult (M _ _ _ $ Nothing0) = ()

public export
Get : (0 formats : List Type) -> (0 val : Type) -> ReqMethod
Get fs v = M GET ok200 fs (Just0 v)

public export
Post : (0 formats : List Type) -> (0 val : Type) -> ReqMethod
Post fs v = M POST ok200 fs (Just0 v)

public export
Put : (0 formats : List Type) -> (0 val : Type) -> ReqMethod
Put fs v = M PUT ok200 fs (Just0 v)

public export
Get' : ReqMethod
Get' = M GET noContent204 [] Nothing0

public export
Post' : ReqMethod
Post' = M POST noContent204 [] Nothing0

public export
Put' : ReqMethod
Put' = M PUT noContent204 [] Nothing0

--------------------------------------------------------------------------------
-- Path
--------------------------------------------------------------------------------

public export
data Part : Type where
  PStr       : String -> Part
  Capture    : (0 t : Type) -> Part

public export
FromString Part where fromString = PStr

public export
0 PartsTypes : List Part -> List Type
PartsTypes []                = []
PartsTypes (PStr _    :: xs) = PartsTypes xs
PartsTypes (Capture t :: xs) = t :: PartsTypes xs

public export
record ReqPath where
  constructor Path
  parts : List Part

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

export
infix 3 ??

public export
data QField : Type where
  (??)  : (name : ByteString) -> (0 type : Type) -> QField
  QBool : (name : ByteString) -> QField

public export
0 QueryTypes : List QField -> List Type
QueryTypes []               = []
QueryTypes ((_ ?? t) :: xs) = t :: QueryTypes xs
QueryTypes (QBool _ :: xs)  = Bool :: QueryTypes xs

public export
0 QueryConstraintTypes : List QField -> List Type
QueryConstraintTypes []               = []
QueryConstraintTypes ((_ ?? t) :: xs) = t :: QueryConstraintTypes xs
QueryConstraintTypes (QBool _ :: xs)  = QueryConstraintTypes xs

public export
record ReqQuery where
  constructor Query
  fields : List QField

--------------------------------------------------------------------------------
-- List Utilities
--------------------------------------------------------------------------------

||| A list of erased (!) types.
|||
||| This is represents the size of a heterogeneous list (it is just
||| a natural number at runtime) and is used to split off a
||| predefined prefix off a heterogeneous list.
public export
data TList : List Type -> Type where
  Nil  : TList []
  (::) : (0 t : Type) -> TList ts -> TList (t::ts)

public export
splitHList : TList ts -> HList (ts ++ rem) -> (HList ts, HList rem)
splitHList []        vs      = ([],vs)
splitHList (t :: ts) (v::vs) =
  let (xs,ys) := splitHList ts vs
   in (v::xs,ys)

||| Proof that a list hold exactly one value.
public export
data Sing : List a -> Type where
  IsSing : Sing [v]

||| Extracts the sole value from a singleton list.
public export
GetSing : (as : List a) -> Sing as => a
GetSing [v] = v

||| Wraps a single value in a heterogeneous list of one value.
public export
wrapSing : (0 ts : List Type) -> (0 prf : Sing ts) => GetSing ts -> HList ts
wrapSing [t] @{IsSing} x = [x]

namespace APIs

  ||| A list of heterogeneous lists, used to describe the APIs of
  ||| a webserver.
  public export
  data APIs : Type where
    Nil  : APIs
    (::) : {0 ts : List Type} -> HList ts -> APIs -> APIs
