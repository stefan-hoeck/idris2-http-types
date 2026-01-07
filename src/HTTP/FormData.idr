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
  name    : ByteString
  content : ByteString

-- public export
-- 0 FormData : Type
-- FormData = SortedMap ByteString FDPart
--
-- crlf : ByteString
-- crlf = "\r\n"
--
-- crlf2 : ByteString
-- crlf2 = "\r\n\r\n"
--
-- pair : ByteString -> (String,String)
-- pair bs =
--   let (x,y) := ByteString.break (58 ==) bs
--    in (toLower $ toString x, toLower . toString . trimLeft $ drop 1 y)
--
-- part : FormData -> ByteString -> FormData
-- part m bs =
--  let (_,r1) := breakDropAtSubstring crlf bs
--      (h,r2) := breakDropAtSubstring crlf2 r2
--   in ?fooo
--       hs    := splitAtSubstring crlf x
--       m     := SortedMap.fromList $ map pair hs
--    in case lookup "content-disposition" m of
--         Just x  =>
--           if x == requestStr
--              then {json := drop 4 y} p
--              else {bytes := drop 4 y} p
--         Nothing => p
--
-- isEnd : ByteString -> Bool
-- isEnd bs = "--" `isPrefixOf` bs
--
-- export
-- multipart : (sep : ByteString) -> ByteString -> FormData
-- multipart sep =
--   let sepBS := "--" <+> sep
--    in foldl part empty . takeWhile (not . isEnd) . splitAtSubstring sepBS
