module HTTP.Prog

import public FS.Posix
import public HTTP.RequestErr
import public IO.Async.Loop.Posix
import public IO.Async.Posix
import public IO.Async.Service

%default total

||| A server programm runs in the `Async` monad and requires
||| polling capabilities.
|||
||| @ es : types of errors a program can fail with
||| @ a  : result type
public export
0 HTTPProg : (es : List Type) -> (a : Type) -> Type
HTTPProg = Async Poll

||| A `Pull` running in the `Async` monad and requiring
||| polling capabilities.
|||
||| @ o  : type of values emitted by the pull
||| @ es : types of errors a pull can fail with
||| @ r  : result type
public export
0 HTTPPull : (o : Type) -> (es : List Type) -> (r : Type) -> Type
HTTPPull = Pull HTTPProg

||| A `Stream` running in the `Async` monad and requiring
||| polling capabilities.
|||
||| A `Stream` is just an alias for a `Pull` with result type `Unit`.
|||
||| @ es : types of errors a pull can fail with
||| @ o  : type of values emitted by the pull
public export
0 HTTPStream : (es : List Type) -> (o : Type) -> Type
HTTPStream = Stream HTTPProg

||| An asynchronous service use to process HTTP requests.
public export
0 HTTPService : (req : Type) -> (resp : req -> Type) -> Type
HTTPService = Service Poll [RequestErr]

||| A handler for error type `e`.
public export
0 ErrorHandler : (e : Type) -> Type
ErrorHandler e = e -> HTTPProg [] ()

public export
0 Handler : Type -> Type
Handler = HTTPProg [RequestErr]
