module HTTP.API.Header

import Data.Buffer
import Data.ByteString
import Derive.Prelude
import HTTP.API.Decode
import HTTP.Status

%default total
%language ElabReflection

public export
record HeaderPart where
  constructor H
  name        : String
  0 type      : Type
  errorStatus : Status

||| Computes the list of types captured by a list of header descriptions.
public export
0 HeaderTypes : List HeaderPart -> List Type
HeaderTypes []              = []
HeaderTypes (H _ t _ :: xs) = t :: HeaderTypes xs

||| Wraps a list of `HeaderPart`s to account for a full description
||| of values extracted from a request's headers.
public export
record ReqHeaders where
  constructor Headers
  headers : List HeaderPart

--------------------------------------------------------------------------------
-- OAuth Tokens
--------------------------------------------------------------------------------

public export
record OAuthToken where
  constructor OAT
  token : ByteString

%runElab derive "OAuthToken" [Show,Eq,Ord]

export
Decode OAuthToken where
  decode bs =
    if "Bearer" `isPrefixOf` bs
       then Right $ OAT $ trimLeft (drop 6 bs)
       else Left (Msg "Invalid bearer token")
