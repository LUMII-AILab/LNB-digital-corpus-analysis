{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BSU
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
--import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
import Text.XML.Expat.SAX as SAX

type Token = BSU.ByteString
type TokenParser = [SAXEvent Token Token] -> [[Token]]

samplefolder = "/Users/pet/Documents/LNBall/02-01-01/LNB-Periodika1/p_001_adve1939n010/"
metsFile = "adve1939n010_mets.xml"
altoFile = "adve1939n010_005_alto.xml"

{-# INLINE blockStart #-}
blockStart :: TokenParser
blockStart ((StartElement "TextBlock" _):xs) = stringStart xs
blockStart (_:xs) = blockStart xs
blockStart [] = []

{-# INLINE stringStart #-}
stringStart :: TokenParser
stringStart ((StartElement "String" attrs):xs) = [getAttribute "CONTENT" attrs] : finish "String" stringStart xs
stringStart ((EndElement "TextBlock"):xs) = blockStart xs
stringStart (_:xs) = stringStart xs
stringStart [] = error "could not find <String>"

getAttribute :: Token -> [(Token, Token)] -> Token
getAttribute attr ((tag, text) : xs) = if (tag == attr) then text else getAttribute attr xs
getAttribute attr [] = error ("could not find attribute " ++ BSU.unpack attr)

{-# INLINE finish #-}
finish :: Token -> TokenParser -> TokenParser
finish tag cont ((EndElement el):xs) | el == tag = cont xs
finish tag cont (_:xs) = finish tag cont xs
finish tag _ [] = error (show (tag,("finish []" :: String)))

main :: IO ()
main = do
  rawContent <- BSL.readFile (samplefolder ++ altoFile)
  let parsed = (blockStart (SAX.parse defaultParseOptions rawContent))
  mapM_ (mapM_ BS.putStrLn) ({- take 5000 -} parsed) -- remove comment to finish early
  putStrLn "Complete!"