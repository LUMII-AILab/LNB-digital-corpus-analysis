{-# LANGUAGE ScopedTypeVariables #-}

module Morphology.Web
(webAnalyze) where

import Network.Curl
import Network.HTTP.Base
import Morphology
import System.IO.Unsafe
import System.Process
import System.IO
import Control.Concurrent
import Control.Exception

morphoserver = "http://lnb.ailab.lv:8182"
--morphoserver = "http://localhost:8182"

-- Main exported function, takes a chunk of text, and passes it to the Java webservice for tokenization and morphological analysis.
webAnalyze :: String -> [Token]
webAnalyze query = 
	let result = unsafePerformIO $ fetchMorphology query -- We believe that calling the the Java webservice is deterministic, read-only, sequence independent and with no observable side-effects.
	in case decodeTokens result of
		Ok tokens -> tokens
		Error err -> error $ "Couldn't analyze \"" ++ query ++ "\" - error " ++ err

-- Queries the java webservice - old simple version, unusable for large queries not fitting int GET request url
fetchMorphology_get text = do
	fetched <- curlGetString (morphoserver ++ "/tokenize/" ++ (urlEncode $ encodeString text)) []
	return $ decodeString . snd $ fetched

-- Queries the java webservice
fetchMorphology :: String -> IO String
fetchMorphology text = curlPostString (encodeString text)

-- covering the gap in Haskell curl library - we need a POST statement that gets back the results and sends JSON paramaters
curlPostString :: String -> IO String
curlPostString query = withCurlDo $ do
	curl <- initialize
	let options = CurlPostFields [ "{\"query\"=" ++ (Text.JSON.encode query) ++ "}" ] : CurlHttpHeaders [ "Content-Type: application/json; charset=UTF-8" ]: method_POST 
	r <- do_curl_ curl (morphoserver ++ "/tokenize") options :: IO CurlResponse
	return $ decodeString $ respBody r