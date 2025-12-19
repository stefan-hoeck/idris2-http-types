module HTTP.API.DSL

import Data.Buffer
import Data.ByteString
import Data.List.Quantifiers
import HTTP.Method
import HTTP.Status

%default total

--------------------------------------------------------------------------------
-- Request Body
--------------------------------------------------------------------------------

public export
data ReqBody : (formats : List Type) -> (val : Type) -> Type where
  Body : (0 fs : List Type) -> (0 res : Type) -> ReqBody fs res

--------------------------------------------------------------------------------
-- Method
--------------------------------------------------------------------------------

public export
record ReqMethod (formats : List Type) (val : Type) where
  constructor M
  method : Method
  status : Status

public export
Get : (0 formats : List Type) -> (0 val : Type) -> ReqMethod formats val
Get _ _ = M GET ok200

public export
Post : (0 formats : List Type) -> (0 val : Type) -> ReqMethod formats val
Post _ _ = M POST ok200

public export
Put : (0 formats : List Type) -> (0 val : Type) -> ReqMethod formats val
Put _ _ = M PUT ok200

public export
record ReqMethod' where
  constructor M'
  method : Method

public export
Get' : ReqMethod'
Get' = M' GET

public export
Post' : ReqMethod'
Post' = M' POST

public export
Put' : ReqMethod'
Put' = M' PUT

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
data RequestPath : List Type -> Type where
  Path : (parts : List Part) -> RequestPath (PartsTypes parts)

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

export
infix 3 ??

public export
data QField : (t : Type) -> Type where
  (??)  : (name : ByteString) -> (0 type : Type) -> QField type
  QBool : (name : ByteString) -> QField Bool

public export
data RequestQuery : (ts : List Type) -> Type where
  Query : All QField ts -> RequestQuery ts

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
