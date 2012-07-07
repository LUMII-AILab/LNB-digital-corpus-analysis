{-# LANGUAGE OverloadedStrings #-}

--import qualified Data.ByteString.Lazy as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T.IO
import qualified Data.Set as Set 
--import Data.Maybe
--import qualified Data.ByteString.Lazy.UTF8 as UTF8



stanfordFolder = "/Users/pet/Dropbox/NER/stanford-ner-2012-04-07/"
taggedFilename = stanfordFolder ++ "production.tagged.txt"
--taggedFilename = stanfordFolder ++ "production.small.txt"

main = do
	start <- getCurrentTime
	--
	file <- T.IO.readFile taggedFilename
	let tokens =  T.lines file
	let entities = Set.toAscList $ Set.fromList $ review Nothing tokens
	mapM (putStrLn . T.unpack) $ entities
	--
	stop <- getCurrentTime
  	putStrLn $ show $ diffUTCTime stop start
  	hFlush stdout
	return ()

review :: Maybe (T.Text, T.Text) -> [T.Text] -> [T.Text]
review previous (x:xs) =
	if x=="" then review previous xs else 
	let 
		tag = (last $ T.split (=='\t') x) 
		word = (head $ T.split (=='\t') x) 
		(ner, remainder) = case previous of 
			Nothing -> ([], if T.length tag > 1 then Just (tag, word) else Nothing)
			Just (prevtag, prevname) -> 
				if T.length tag > 1 then 
					if tag == prevtag then ([], Just (tag, prevname `T.append` " " `T.append` word)) else ([prevtag `T.append` " " `T.append` prevname], Just (tag, word))
				else ([prevtag `T.append` " " `T.append` prevname], Nothing)
	in ner ++ review remainder xs
review _ [] = []