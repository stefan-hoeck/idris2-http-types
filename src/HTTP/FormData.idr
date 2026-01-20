module HTTP.FormData

import Data.Buffer
import Data.ByteString
import Data.List
import Data.SortedMap
import HTTP.Header

%default total

public export
record FDPart where
  constructor FDP
  headers : Headers
  name    : String
  content : ByteString

public export
0 FormData : Type
FormData = List FDPart

crlf : ByteString
crlf = "\r\n"

crlf2 : ByteString
crlf2 = "\r\n\r\n"

part : ByteString -> Maybe FDPart
part bs = Prelude.do
  guard (not $ "--" `isPrefixOf` bs)
  let (_,r1) := breakDropAtSubstring crlf bs
      n      := substringIndex crlf2.repr r1.repr
  (rh,r2)    <- splitAt (n.fst + crlf2.size) r1
  h          <- parseHeadersMay rh
  cd         <- contentDisposition h
  nm         <- parameter "name" cd.params
  Just $ FDP h nm r2

export
multipart : (sep : ByteString) -> ByteString -> FormData
multipart sep bs =
  let sepBS := crlf <+> "--" <+> sep
   in mapMaybe part (splitAtSubstring sepBS bs)
