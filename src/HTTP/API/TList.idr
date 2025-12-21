module HTTP.API.TList

import public Data.List.Quantifiers

%default total

||| A list of erased (!) types.
|||
||| This is represents the size of a heterogeneous list (it is just
||| a natural number at runtime) and is used to split off a
||| predefined prefix off a heterogeneous list.
public export
data TList : List Type -> Type where
  Nil  : TList []
  (::) : (0 t : Type) -> TList ts -> TList (t::ts)

||| We can split a concatenation of heterogeneous lists by removing
||| a fixed-sized prefix.
|||
||| As a witness of the size of the prefix, a value of type `TList`
||| is required.
public export
splitHList : TList ts -> HList (ts ++ rem) -> (HList ts, HList rem)
splitHList []        vs      = ([],vs)
splitHList (t :: ts) (v::vs) =
  let (xs,ys) := splitHList ts vs
   in (v::xs,ys)

||| Concatenates two `TList`s.
public export
(++) : TList xs -> TList ys -> TList (xs++ys)
(++) (t::ts) tys = t :: (ts ++ tys)
(++) []      tys = tys
