module HTTP.MimeType

import Data.ByteString
import Data.String

%default total

public export
record MimeType where
  constructor MT
  type  : String
  param : Maybe (String,String)

export
toMimeType : ByteString -> Maybe MimeType
toMimeType bs =
  case trim <$> split 59 bs of -- 59 = ;
    [mt,p] => case (ByteString.toString . trim) <$> split 61 p of
      [pa,v] => Just (MT (toLower $ toString mt) (Just (toLower pa, v)))
      _      => Nothing
    [mt]   => Just (MT (toLower $ toString mt) Nothing)
    _      => Nothing
