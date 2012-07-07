{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BC
import qualified Data.ByteString.Lazy as BSL
import Text.XML.Expat.SAX as SAX
import System.IO
import Data.String.Utils
import Data.Maybe
import Data.List
import Data.List.Split
import Text.JSON
import Text.ParserCombinators.Parsec as Parsec
import qualified Morphology 
import System.IO.Unsafe
import Control.Monad
import System.Environment

data Task = PrepareTrainData | PrepareTestData deriving (Show, Eq)

defaultTask = "train_data"

oldFiles = ["1861 Latviešu Avīzes.xml","1882 Arājs.xml","1928 Atpūta.xml"]
trainFiles = oldFiles ++ ["1935 Mūzikas Apskats.xml","1942 Austrumu apgabala raidītāju grupa.xml","1988 Padomju Jaunatne.xml","2007 Dadzis.xml","G 1934 Madona.xml","G 1966 Oskars Kalpaks.xml","G 2005 Ugāles Baznīca.xml"]
testFiles = ["1999 Zīlīte.xml", "G 1998 Kārlis Ulmanis.xml", "1918 Baltijas Ziņas.xml"]

getTag :: [NERTag] -> String
getTag [] = "O"
getTag tags = nerClass $ last tags
getTag tags = 
	if find (\x -> nerClass x == "PERSONA") tags == Nothing
		then "O"
		else "PERSON"

allowedTags = ["PERSONA","LOKACIJA","ORGANIZACIJA"]

filterTags :: [NERTag] -> [NERTag]
filterTags (x:xs) = 
	if nerClass x `elem` allowedTags then x : filterTags xs
		else filterTags xs
filterTags x = x

dataFolder = "/Users/pet/Dropbox/NER/Ex5/"
xmlFolder = "/Users/pet/Dropbox/NER/Etalona dati/No web 15 jun/"
stanfordFolder = "/Users/pet/Dropbox/NER/stanford-ner-2012-04-07/"
trainFilename = stanfordFolder ++ "train_all.txt"
compareFolder = "/Library/Webserver/Documents/compare/compare/"

type XMLToken = BC.ByteString
unpack :: XMLToken -> String
unpack x = BC.toString x

data NERTag = NERTag {nerClass :: String, nerType :: String} deriving (Show, Eq)
data TextToken = Token {word :: String, spaceBefore :: Bool, nerTags :: [NERTag], lemma :: String, postag :: String} deriving (Show) -- lemma un postag

getAttribute :: XMLToken -> [(XMLToken, XMLToken)] -> String
getAttribute attr ((tag, text) : xs) = if (tag == attr) then (BC.toString text) else getAttribute attr xs
getAttribute attr [] = error ("could not find attribute '" ++ BC.toString attr ++ "'")

getAttributeOptional :: XMLToken -> [(XMLToken, XMLToken)] -> String
getAttributeOptional attr ((tag, text) : xs) = if (tag == attr) then (BC.toString text) else getAttributeOptional attr xs
getAttributeOptional attr [] = ""

main = do
	args <- getArgs
	let task = if length args > 0 then head args else defaultTask
	case task of
		"train_data" -> do
			writeFile trainFilename ""
			mapM (convertXMLFile PrepareTrainData) trainFiles
		"test_data" -> do
			mapM (convertXMLFile PrepareTestData) testFiles
		"convert_tagged" -> do
			mapM (readVert . replace ".xml" ".tagged.txt") testFiles
		_ -> do
			putStrLn "Please use a task as an argument"
			putStrLn "train_data: convert training data"
			putStrLn "test_data: convert test data"
			putStrLn "convert_tagged: convert ner-tagged results back"
			return [()]
	return ()

convertXMLFile task filename = do
	(inp,out,err,pid) <- Morphology.pipeInit
	let analyze = (Morphology.pipeAnalyze inp out)	
	tokens <- readXML analyze $ xmlFolder ++ filename
	when (task == PrepareTrainData) $ appendFile trainFilename $ unlines $ map (writeStanfordToken analyze True) tokens   
	when (task == PrepareTestData) $ writeFile (stanfordFolder ++ replace ".xml" ".txt" filename) $ unlines $ map (writeStanfordToken analyze False) tokens   
	when (task == PrepareTestData) $ writeFile (compareFolder ++ "human/" ++ filename) $ writeNERXML tokens
	--writeFile (replace ".xml" "_bonito.xml" filename) $ writeBonitoXML analyze tokens
	--writeFile (replace ".xml" "_bonito2.xml" filename) $ writeBonito2XML analyze tokens
	Morphology.pipeClose inp pid

readXML analyze filename = do
    docText <- B.readFile filename
    return $ getText analyze (SAX.parse defaultParseOptions $ BSL.fromChunks [docText]) 

-- Input format - XML used in NER markup GUI
getText :: (String -> [Morphology.Token]) -> [SAXEvent XMLToken XMLToken] -> [TextToken]
getText analyze ((StartElement "p" _):xs) = getP analyze [] True xs
getText analyze (_:xs) = getText analyze xs
getText _ [] = []

getP :: (String -> [Morphology.Token]) -> [NERTag] -> Bool -> [SAXEvent XMLToken XMLToken] -> [TextToken]
getP analyze tags trailingSpace ((CharacterData txt):xs ) = 
	let (tokens, newTrailingSpace) = tagTokens analyze (unpack txt) trailingSpace tags 
	in tokens ++ getP analyze tags newTrailingSpace xs
getP analyze tags trailingSpace ((StartElement tag attrs):xs) = getP analyze (NERTag {nerClass = unpack tag, nerType = getAttributeOptional "type" attrs} : tags) trailingSpace xs
getP analyze _ trailingSpace ((EndElement "p"):xs) = Token {word = "<p/>", spaceBefore = trailingSpace, nerTags=[], lemma = "<p/>", postag = "-"} : getText analyze xs
getP analyze (tag:tags) trailingSpace ((EndElement tag2):xs) = getP analyze (if nerClass tag == unpack tag2 then tags else tag:tags) trailingSpace xs
getP analyze tags trailingSpace (_:xs) = getP analyze tags trailingSpace xs
getP _ _ _ [] = []

tagTokens :: (String -> [Morphology.Token]) -> String -> Bool -> [NERTag] -> ([TextToken], Bool)
tagTokens analyze text trailingSpace tags =
	if (strip $ text) == "" 
		then ([], trailingSpace || (length text > 0))
		else let 
			(token:tokens) = map (\x -> (convertToken x){nerTags = filterTags tags}) $ analyze text 
			-- TODO - normālu tokenizāciju !!! neder words text !!! un šitajos tokenos tad var ielikt uzreiz arī morfotagus!			
			in ( token{spaceBefore = trailingSpace || startswith " " text} : tokens, endswith " " text)

convertToken :: Morphology.Token -> TextToken
convertToken token =
	Token {word = replace " " "_" $ Morphology.word token, spaceBefore = True, nerTags = [], lemma = replace " " "_" $ Morphology.lemma token, postag = Morphology.tag token}

-- Output format - vertical list of tokens, including the tags if used for training NER model.			
writeStanfordToken :: (String -> [Morphology.Token]) -> Bool -> TextToken -> String
writeStanfordToken analyze includeTag token = 
	let 
		--mtoken = head $ analyze $ word token  --TODO - čekot vai nav tukšs saraksts
		--mtag = Morphology.tag mtoken
		--postag = if length mtag > 0 then [head mtag] else "-"
		mtag = postag token
		mpostag = if length mtag > 0 then [head mtag] else "-"
	in (if (spaceBefore token) || (word token == "<p/>")
		then ""
		else if includeTag
			then "<g/>\t-\t<g/>\t-\tO\n"
			else "<g/>\n"
	)
	++ word token ++ "\t" ++ mpostag ++ "\t" ++ lemma token ++ "\t" ++ mtag
	++ if includeTag then "\t" ++ getTag (nerTags token) else ""

-- Output format - XML used in NER markup GUI
writeNERXML :: [TextToken] -> String
writeNERXML tokens = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<TEXT>\n" ++ (Data.String.Utils.join "\n" $ map writeP $ splitWhen (\x -> word x == "<p/>") tokens) ++ "</TEXT>"

writeP :: [TextToken] -> String
writeP tokens = "<p>" ++ writeXMLTokens tokens True [] ++ "</p>"

writeXMLTokens :: [TextToken] -> Bool -> [NERTag] -> String
writeXMLTokens (token:tokens) skipSpace tags =
	writeTagDifference tags (nerTags token) (skipSpace || not (spaceBefore token)) ++ 
	word token ++
	writeXMLTokens tokens False (nerTags token)
writeXMLTokens [] _ tags = writeTagDifference tags [] True

writeTagDifference :: [NERTag] -> [NERTag] -> Bool -> String
writeTagDifference oldTags newTags skipSpace
	| oldTags == newTags = if skipSpace then "" else " "
	| not (isSuffixOf oldTags newTags) = -- ir kāds vecs tags, kas jāaizver
		"</" ++ (nerClass $ head oldTags) ++ ">" ++ writeTagDifference (tail oldTags) newTags skipSpace
	| otherwise = -- ir kāds jauns tags, kas jāatver
		(if skipSpace then "" else " ") ++
		"<" ++ (nerClass $ last newTags) ++ 
		(if (nerType $ last newTags) == "" then ""
		else " type=\"" ++ (nerType $ last newTags) ++ "\"") ++ 
		">" ++ writeTagDifference oldTags (init newTags) True

-- Output format - vert format used in the Bonito corpus
writeBonitoXML :: (String -> [Morphology.Token]) -> [TextToken] -> String
writeBonitoXML analyze tokens = 
    "<doc>\n<p>\n" ++
    unlines (map writeBonitoToken $ morphoAnalyze analyze tokens) ++
    "</p></doc>\n"

writeBonitoToken :: TextToken -> String
writeBonitoToken token = 
	let tok = word token in
	(if (spaceBefore token) || (tok == "<p/>")
		then ""
		else "<g />\n"
	)
	++ 
	if tok == "<p/>" then "</p>\n<p>\n"
	else word token ++ "\t" ++ lemma token ++ "\t" ++ postag token ++ "\t" ++ (getTag $ nerTags token)

writeBonito2XML :: (String -> [Morphology.Token]) -> [TextToken] -> String
writeBonito2XML analyze tokens = 
    "<doc>\n<p>\n" ++
    writeBonito2Tokens (morphoAnalyze analyze tokens) [] ++
    "</p></doc>\n"

writeBonito2Tokens :: [TextToken] -> [NERTag] -> String
writeBonito2Tokens (token:tokens) oldTags = 
	let tok = word token in
	(writeTagDifference2 oldTags $ nerTags token) ++
	(if (spaceBefore token) || (tok == "<p/>")
		then ""
		else "<g />\n"
	)
	++ 
	(if tok == "<p/>" then "</p>\n<p>\n"
	else word token ++ "\t" ++ lemma token ++ "\t" ++ postag token)
	++ "\n"
	++ (writeBonito2Tokens tokens $ nerTags token)
writeBonito2Tokens [] _ = []

writeTagDifference2 :: [NERTag] -> [NERTag] -> String
writeTagDifference2 oldTags newTags 
	| oldTags == newTags = ""
	| not (isSuffixOf oldTags newTags) = -- ir kāds vecs tags, kas jāaizver
		"</" ++ (nerClass $ head oldTags) ++ ">\n" ++ writeTagDifference2 (tail oldTags) newTags
	| otherwise = -- ir kāds jauns tags, kas jāatver
		"<" ++ (nerClass $ last newTags) ++ 
		(if (nerType $ last newTags) == "" then ""
		else " type=\"" ++ (nerType $ last newTags) ++ "\"") ++ 
		">\n" ++ writeTagDifference2 oldTags (init newTags) 

--------- Parsec analizators Stanford vert-formaatam
stanfordFile = Parsec.endBy line eol
line = do
	columns <- Parsec.sepBy cell (char '\t')
	let tag = if length columns >= 2 then columns!!2 else ""
	return Token{word = columns!!0, spaceBefore = True, nerTags = if tag == "O" then [] else [NERTag{nerClass = tag, nerType = ""}], lemma = "", postag = ""} --FIXME - tur būs arī lemmas un postagi
cell = many (noneOf "\t\n")
eol = char '\n'

treatNospaces :: [TextToken] -> [TextToken]
treatNospaces (x:y:xs) = if (word x == "<g/>") 
	then y{spaceBefore = False} : (treatNospaces xs)
	else x : (treatNospaces (y:xs))
treatNospaces x = x

readVert filename = do
    docText <- readFile $ stanfordFolder ++ filename
    case Parsec.parse stanfordFile "(unk)" docText of
    	Left err -> putStrLn $ show err
    	Right x -> do
    		writeFile (compareFolder ++ "pc/" ++ replace ".tagged.txt" ".xml" filename) $ writeNERXML $ treatNospaces x
    		--putStrLn $ show x
    return ()

-- Morphology interface
morphoAnalyze :: (String -> [Morphology.Token]) -> [TextToken] -> [TextToken]
morphoAnalyze analyze tokens = map (morphoAnalyzeToken analyze) tokens
 
morphoAnalyzeToken :: (String -> [Morphology.Token]) -> TextToken -> TextToken
morphoAnalyzeToken analyze token = let morphotok = head $ analyze $ word token --FIXME - assumption ka būs tieši 1 tokens
	in token{lemma = Morphology.lemma morphotok, postag = Morphology.tag morphotok}