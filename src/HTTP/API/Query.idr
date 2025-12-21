module HTTP.API.Query

import Data.ByteString

%default total

export
infix 3 ??

||| Mathes a single key-value pair in the *query* part of
||| the request URI.
|||
||| In general, key and value are assumed to be separated by an
||| equals sign ('='), which several key-value pairs are
||| separated by an ampersand ('&').
|||
||| In addition to key-value pair, boolean queries are also supported,
||| where only the presence of the key is tested, and a value can be
||| optional.
public export
data QField : Type where
  (??)  : (name : ByteString) -> (0 type : Type) -> QField
  QBool : (name : ByteString) -> QField

||| Computes the list of types captured by a URI query description.
public export
0 QueryTypes : List QField -> List Type
QueryTypes []               = []
QueryTypes ((_ ?? t) :: xs) = t :: QueryTypes xs
QueryTypes (QBool _ :: xs)  = Bool :: QueryTypes xs

||| Computes the list of types that need to be decoded from
||| a URI query part.
|||
||| This is used to compute the constraints required to interpret a
||| query description.
|||
||| This differs from `QueryTypes`, since boolean queries need not
||| be decoded.
public export
0 QueryConstraintTypes : List QField -> List Type
QueryConstraintTypes []               = []
QueryConstraintTypes ((_ ?? t) :: xs) = t :: QueryConstraintTypes xs
QueryConstraintTypes (QBool _ :: xs)  = QueryConstraintTypes xs

||| Wraps a list of `QField`s to account for a full description
||| of values extracted from a URI query.
public export
record ReqQuery where
  constructor Query
  fields : List QField
