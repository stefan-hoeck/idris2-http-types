module HTTP.API.Server

import public HTTP.Header
import public HTTP.Prog
import public HTTP.Request
import public HTTP.Response
import public HTTP.Status
import public HTTP.API.DSL
import public HTTP.API.Decode

%default total

||| Interface for building up a server API from a
||| heterogeneous list of values.
public export
interface Serve (0 a : Type) where
  0 Constraint : a -> Type
  0 InTypes    : a -> List Type
  0 OutTypes   : a -> List Type
  outs         : (v : a) -> TList (OutTypes v)
  canHandle    : a -> Request -> Bool

  fromRequest  :
       (v : a)
    -> {auto con : Constraint v}
    -> Request
    -> Handler (HList $ InTypes v)

  adjResponse  :
       (v : a)
    -> {auto con : Constraint v}
    -> HList (OutTypes v)
    -> Request
    -> Response
    -> Handler Response

public export
0 Constraints : All Serve ts => HList ts -> List Type
Constraints @{[]}   []      = []
Constraints @{_::_} (v::vs) = Constraint v :: Constraints vs

public export
0 AllInTypes : All Serve ts => HList ts -> List Type
AllInTypes @{[]}   []      = []
AllInTypes @{_::_} (v::vs) = InTypes v ++ AllInTypes vs

public export
0 AllOutTypes : All Serve ts => HList ts -> List Type
AllOutTypes @{[]}   []      = []
AllOutTypes @{_::_} (v::vs) = OutTypes v ++ AllOutTypes vs

public export
0 Fun : List Type -> Type -> Type
Fun []        r = r
Fun (t :: ts) r = t -> Fun ts r

public export
0 API : All Serve ts => (vs : HList ts) -> Sing (AllOutTypes vs) => Type
API vs = Fun (AllInTypes vs) (Handler (GetSing $ AllOutTypes vs))

getIns :
     {auto all : All Serve ts}
  -> (vs : HList ts)
  -> {auto con : HList (Constraints vs)}
  -> Request
  -> Handler (HList $ AllInTypes vs)
getIns @{[]}   []      @{[]}   req = pure []
getIns @{_::_} (v::vs) @{_::_} req = Prelude.do
  rs  <- fromRequest v req
  rem <- getIns vs req
  pure (rs ++ rem)

applyAPI :
     HList ts
  -> (0 os : List Type)
  -> {auto 0 prf : Sing os}
  -> Fun ts (Handler (GetSing os))
  -> Handler (HList os)
applyAPI []        os r = map (wrapSing os) r
applyAPI (v :: vs) os f = applyAPI vs os (f v)

putOuts :
     {auto all : All Serve ts}
  -> (vs : HList ts)
  -> {auto con : HList (Constraints vs)}
  -> HList (AllOutTypes vs)
  -> Request
  -> Response
  -> Handler Response
putOuts @{[]}   []      [] @{[]}   req resp = pure resp
putOuts @{_::_} (x::xs) vs @{_::_} req resp = Prelude.do
  let (ts,rem) := splitHList (outs x) vs
  r2 <- adjResponse x ts req resp
  putOuts xs rem req r2

public export
data Server : APIs -> Type where
  Nil  : Server []
  (::) :
       {0 ts       : List Type}
    -> {0 as       : APIs}
    -> {hl         : HList ts}
    -> {auto all   : All Serve ts}
    -> {auto 0 prf : Sing (AllOutTypes hl)}
    -> {auto all   : HList (Constraints hl)}
    -> API hl
    -> Server as
    -> Server (hl :: as)

canServe : {auto all : All Serve ts} -> HList ts -> Request -> Bool
canServe @{[]}    []      req = True
canServe @{s::ss} (v::vs) req = canHandle v req && canServe vs req

serve1 :
     {auto all   : All Serve ts}
  -> (api        : HList ts)
  -> {auto 0 prf : Sing (AllOutTypes api)}
  -> {auto con   : HList (Constraints api)}
  -> API api
  -> Request
  -> Handler Response
serve1 api f req = Prelude.do
  ins  <- getIns api req
  outs <- applyAPI ins (AllOutTypes api) f
  putOuts api outs req empty

export
serveAll : (0 apis   : APIs) -> Server apis -> Request -> Handler Response
serveAll []         []              req = throw (requestErr notFound404)
serveAll (hl :: as) ((::) {hl} f x) req =
  case canServe hl req of
    True  => serve1 hl f req
    False => serveAll as x req

--------------------------------------------------------------------------------
-- Body
--------------------------------------------------------------------------------

decodeBody : All (`DecodeVia` t) ts -> Request -> Either RequestErr (HList [t])
decodeBody []        r = Left $ requestErr unsupportedMediaType415
decodeBody (d :: ds) r =
  case hasContentType r.headers (mediaType @{d}) of
    False => decodeBody ds r
    True  =>
     bimap
       (decodeErr unsupportedMediaType415)
       (\x => [x])
       (decodeVia @{d} r.content)

public export
Serve ReqBody where
  Constraint b         = All (`DecodeVia` b.result) b.formats
  InTypes  b           = [b.result]
  OutTypes _           = []
  outs     _           = %search
  canHandle   _ r      = True
  fromRequest _ r      = injectEither $ decodeBody con r
  adjResponse _ [] req = pure
