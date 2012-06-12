module LNB.Wikipedia where

import Network.Curl
import Network.HTTP.Base
import Text.JSON as J

main = readEntities

readEntities = do
	f <- readFile "/Users/pet/Documents/nerDBfilter.txt"
	mapM checkEntity $ lines f
	return ()


--(!) = flip valFromObj

checkEntity item = do
	wiki <- curlGetString ("http://lv.wikipedia.org/w/api.php?action=query&prop=extracts&titles=" ++ urlEncode item ++ "&format=json&exintro=1") [CurlUserAgent "Experiment PeterisP@gmail.com"]
	putStrLn $ show $ snd wiki 
	let json = J.decode $ snd wiki :: Result (JSObject JSValue)
	putStrLn $ show $ json
	case json of
		Error e -> return ()
		Ok xx -> putStrLn $ decodeWiki xx
	--let query = (snd json) ! "query"
	--putStrLn $ show $ query

decodeWiki :: JSObject JSValue -> String
decodeWiki wiki = 
	let (!) = flip valFromObj in 
		show $ (wiki ! "query" :: Result (JSObject JSValue)) 



-- Takes a JSON Object with SemTi attribute-value structures, and extracts word/tag/lemma core attributes
--decodeToken :: JSObject JSValue -> Result Token
--decodeToken token = let (!) = flip valFromObj in do
--    word <- token ! "Vārds"
--    lemma <- token ! "Pamatforma"
--    tag <- token ! "Marķējums"
--    return Token {word = word, lemma = lemma, tag = tag}

-- Does the same for a whole array of tokens
--decodeTokens :: String -> Result [Token]
--decodeTokens = Text.JSON.decode >=> mapM decodeToken
