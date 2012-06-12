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

includeTag = False

getTag :: [NERTag] -> String
getTag [] = "O"
getTag tags = nerClass $ last tags
getTag tags = 
	if find (\x -> nerClass x == "PERSONA") tags == Nothing
		then "O"
		else "PERSON"

filterTags :: [NERTag] -> [NERTag]
filterTags (x:xs) = 
	if nerClass x == "PERSONA" then x : filterTags xs
		else filterTags xs
filterTags x = x

dataFolder = "/Users/pet/Dropbox/NER/Mans NER/"

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
	--mapM convertXMLFile ["G 1966 Oskars Kalpaks.xml","G 2005 Ugāles Baznīca.xml","G 1998 Kārlis Ulmanis.xml"]
	mapM (convertXMLFile . (++) dataFolder) ["G 2005 Ugāles Baznīca.xml"]
	--readVert $ dataFolder ++ "tagged.txt"
	return ()
	--readXML "G 1966 Oskars Kalpaks.xml"

convertXMLFile filename = do
	tokens <- readXML filename
	(inp,out,err,pid) <- Morphology.pipeInit
	--mapM (putStrLn . show) tokens
	writeFile (replace ".xml" ".txt" filename) $ unlines $ map writeStanfordToken tokens
	writeFile (replace ".xml" "_changed.xml" filename) $ writeNERXML tokens
	writeFile (replace ".xml" "_bonito.xml" filename) $ writeBonitoXML (Morphology.pipeAnalyze inp out) tokens
	writeFile (replace ".xml" "_bonito2.xml" filename) $ writeBonito2XML (Morphology.pipeAnalyze inp out) tokens
	Morphology.pipeClose inp pid

readXML filename = do
    docText <- B.readFile filename
    return $ getText (SAX.parse defaultParseOptions $ BSL.fromChunks [docText]) 

-- Input format - XML used in NER markup GUI
getText :: [SAXEvent XMLToken XMLToken] -> [TextToken]
getText ((StartElement "p" _):xs) = getP [] True xs
getText (_:xs) = getText xs
getText [] = []

getP :: [NERTag] -> Bool -> [SAXEvent XMLToken XMLToken] -> [TextToken]
getP tags trailingSpace ((CharacterData txt):xs ) = 
	let (tokens, newTrailingSpace) = tagTokens (unpack txt) trailingSpace tags 
	in tokens ++ getP tags newTrailingSpace xs
getP tags trailingSpace ((StartElement tag attrs):xs) = getP (NERTag {nerClass = unpack tag, nerType = getAttributeOptional "type" attrs} : tags) trailingSpace xs
getP _ trailingSpace ((EndElement "p"):xs) = Token {word = "<p/>", spaceBefore = trailingSpace, nerTags=[], lemma = "", postag = ""} : getText xs
getP (tag:tags) trailingSpace ((EndElement tag2):xs) = getP (if nerClass tag == unpack tag2 then tags else tag:tags) trailingSpace xs
getP tags trailingSpace (_:xs) = getP tags trailingSpace xs
getP _ _ [] = []

tagTokens :: String -> Bool -> [NERTag] -> ([TextToken], Bool)
tagTokens text trailingSpace tags =
	if (strip $ text) == "" 
		then ([], trailingSpace || (length text > 0))
		else let 
			(token:tokens) = map (\x -> Token {word = x, spaceBefore = True, nerTags = filterTags tags, lemma = "", postag = ""}) $ words text 
			in ( token{spaceBefore = trailingSpace || startswith " " text} : tokens, endswith " " text)

-- Output format - vertical list of tokens, including the tags if used for training NER model.			
writeStanfordToken :: TextToken -> String
writeStanfordToken token = 
	(if (spaceBefore token) || (word token == "<p/>")
		then ""
		else if includeTag
			then "<g/>\tO\n"
			else "<g/>\n"
	)
	++ if includeTag
		then word token ++ "\t" ++ getTag (nerTags token)
		else word token

-- Output format - XML used in NER markup GUI
writeNERXML :: [TextToken] -> String
writeNERXML tokens = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<TEXT>\n" ++ (join "\n" $ map writeP $ splitWhen (\x -> word x == "<p/>") tokens) ++ "</TEXT>"

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
	let tag = columns!!2
	return Token{word = columns!!0, spaceBefore = True, nerTags = if tag == "O" then [] else [NERTag{nerClass = tag, nerType = ""}], lemma = "", postag = ""} --FIXME - tur būs arī lemmas un postagi
cell = many (noneOf "\t\n")
eol = char '\n'

treatNospaces :: [TextToken] -> [TextToken]
treatNospaces (x:y:xs) = if (word x == "<g/>") 
	then y{spaceBefore = False} : (treatNospaces xs)
	else x : (treatNospaces (y:xs))
treatNospaces x = x

readVert filename = do
    docText <- readFile filename
    case Parsec.parse stanfordFile "(unk)" docText of
    	Left err -> putStrLn $ show err
    	Right x -> do
    		writeFile (replace ".txt" "_reviewed.xml" filename) $ writeNERXML $ treatNospaces x
    		putStrLn $ show x
    return ()

-- Morphology interface
morphoAnalyze :: (String -> [Morphology.Token]) -> [TextToken] -> [TextToken]
morphoAnalyze analyze tokens = map (morphoAnalyzeToken analyze) tokens
 
morphoAnalyzeToken :: (String -> [Morphology.Token]) -> TextToken -> TextToken
morphoAnalyzeToken analyze token = let morphotok = head $ analyze $ word token --FIXME - assumption ka būs tieši 1 tokens
	in token{lemma = Morphology.lemma morphotok, postag = Morphology.tag morphotok}