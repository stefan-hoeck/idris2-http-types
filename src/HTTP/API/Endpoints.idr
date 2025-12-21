module HTTP.API.Endpoints

import public Data.List.Quantifiers

%default total

||| A list of heterogeneous lists, used to describe the APIs of
||| a webserver.
public export
data Endpoints : Type where
  Nil  : Endpoints
  (::) : {0 ts : List Type} -> HList ts -> Endpoints -> Endpoints

||| Concatenates two APIs.
public export
(++) : Endpoints -> Endpoints -> Endpoints
(++) (a::as) bs = a :: (as ++ bs)
(++) []      bs = bs
