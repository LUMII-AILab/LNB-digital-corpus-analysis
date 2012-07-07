{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Morphology 
(cheapAnalyze, nonLVAnalyze, nonLVAnalyze_T, pipeAnalyze, pipeAnalyze_T, pipeInit, pipeClose, Token(Token), word, lemma, tag, Token_T(Token_T), word_t, lemma_t, tag_t) where

import Codec.Binary.UTF8.String
import Control.Monad
import System.IO.Unsafe
import System.Process
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.List.Split
import Control.Concurrent
import Control.Exception

data Token = Token {word :: String, lemma :: String, tag :: String}
	deriving (Show, Eq)

data Token_T = Token_T {word_t :: T.Text, lemma_t :: T.Text, tag_t :: T.Text}
	deriving (Show, Eq)


pipetool = "/Users/pet/Documents/java/Webservices/pipetool.sh -tab"

cheapAnalyze :: String -> [Token]
cheapAnalyze query = map (\x -> Token {word = x, lemma = x, tag = ""}) $ words query

nonLVAnalyze :: String -> [Token]
nonLVAnalyze query = map (\x -> Token {word = x, lemma = x, tag = "xf"}) $ words query

nonLVAnalyze_T :: String -> [Token_T]
nonLVAnalyze_T query = map (\x -> Token_T {word_t = T.pack x, lemma_t = T.pack x, tag_t = "xf"}) $ words query

pipeAnalyze :: Handle -> Handle -> String -> [Token]
pipeAnalyze inp out query = 
	let result = unsafePerformIO $ pipeRequest inp out query -- We believe that calling the the piped Java tool is deterministic, read-only, sequence independent and with no observable side-effects.
	in decodeTokens result

pipeAnalyze_T :: Handle -> Handle -> String -> [Token_T]
pipeAnalyze_T inp out query = 
	decodeTokens_T $ unsafePerformIO $ pipeRequest_T inp out query -- We believe that calling the the piped Java tool is deterministic, read-only, sequence independent and with no observable side-effects.

pipeRequest :: Handle -> Handle -> String -> IO String
pipeRequest _ _ "" = return "[]"
pipeRequest inp out query = do
	--putStrLn query  -- debuginfo
	--hFlush stdout
	hPutStrLn inp query
	Control.Exception.catch (hGetLine out) pipeError

pipeRequest_T :: Handle -> Handle -> String -> IO T.Text
pipeRequest_T _ _ "" = return "[]"
pipeRequest_T inp out query = do
	hPutStrLn inp query
	Control.Exception.catch (T.IO.hGetLine out) pipeError_T

pipeInit :: IO (Handle, Handle, Handle, ProcessHandle)
pipeInit = Control.Exception.catch ( 
	do
	(inp,out,err,pid) <- runInteractiveCommand pipetool 
	hSetBinaryMode inp False
	hSetBinaryMode out False
	hSetBuffering inp LineBuffering
	hSetBuffering out LineBuffering
	line <- hGetLine out -- 2 header lines about loading lexicon
	--putStrLn line
	line <- hGetLine out
	--putStrLn line
	hFlush stdout
	--logfails <- openFile "/Users/pet/Documents/testdata.txt" WriteMode
	return (inp,out,err,pid)
	)
	(\(e :: SomeException) -> do
		putStrLn $ "Exception at init of Java morphology piping : " ++ (show e)
		error "Cannot instantiate morphology pipe")

pipeClose :: Handle -> ProcessHandle -> IO ()
pipeClose inp pid = do
	hPutStrLn inp ""
	terminateProcess pid

pipeError :: IOError -> IO String
pipeError e = do
	putStrLn $ "IOError at Java morphology piping : " ++ (show e)
	return "[]" -- empty JSON list

pipeError_T :: IOError -> IO T.Text
pipeError_T e = do
	putStrLn $ "IOError at Java morphology piping : " ++ (show e)
	return "[]" -- empty JSON list

-- maps a tab-separated word-tag-lemma string to the token structure
decodeToken :: [String] -> Token
decodeToken fields = Token {word = head fields, lemma = if length fields>=2 then fields !! 2 else "", tag = if length fields>=1 then fields !! 1 else "" }

decodeToken_T :: [T.Text] -> Token_T
decodeToken_T fields = Token_T {word_t = head fields, lemma_t = if length fields>=2 then fields !! 2 else "", tag_t = if length fields>=1 then fields !! 1 else "" }

-- Does the same for a whole array of tokens
decodeTokens :: String -> [Token]
decodeTokens text = map decodeToken $ splitEvery 3 $ splitOn "\t" text

decodeTokens_T :: T.Text -> [Token_T]
decodeTokens_T text = map decodeToken_T $ splitEvery 3 $ T.splitOn "\t" text