{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module ParseALTO 
(getALTOPage, XMLToken, getAttribute, pageVertXML, pageVertXML_T, pageStanfordVert, pageTagSoupXML, pagePlaintext, Page(Page), TextBlock(TextBlock), gTag, formatVertToken, tokens, blocks, xmlEscape) where

import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as BC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Text.XML.Expat.SAX as SAX
import Data.String.Utils
import Data.Char
import Morphology(Token(Token), word, lemma, tag, Token_T(Token_T), word_t, lemma_t, tag_t)
import System.IO
import Control.DeepSeq

data Page = Page {nr :: String, blocks :: [TextBlock]}
    deriving (Show, Eq)
data TextBlock = TextBlock {block_id :: String, tokens :: String}
    deriving (Show, Eq)
data ArticleMeta = ArticleMeta {label :: String}

type XMLToken = BC.ByteString
getAttribute :: XMLToken -> [(XMLToken, XMLToken)] -> String
getAttribute attr ((tag, text) : xs) = if (tag == attr) then (BC.toString text) else getAttribute attr xs
getAttribute attr [] = error ("could not find attribute " ++ BC.toString attr)

getAttributeOptional :: XMLToken -> [(XMLToken, XMLToken)] -> String
getAttributeOptional attr ((tag, text) : xs) = if (tag == attr) then (BC.toString text) else getAttributeOptional attr xs
getAttributeOptional attr [] = ""

getALTOPage :: String -> IO Page
getALTOPage filename = do
    docText <- BS.readFile filename
    let page = pageStart (SAX.parse defaultParseOptions $ BSL.fromChunks [docText]) 
    return page

pageStart :: [SAXEvent XMLToken XMLToken] -> Page
pageStart ((StartElement "Page" attrs):xs) = Page{ nr = getAttribute "PHYSICAL_IMG_NR" attrs, blocks = getBlock xs}
pageStart (_:xs) = pageStart xs
pageStart [] = error "could not find <Page>" 

getBlock :: [SAXEvent XMLToken XMLToken] -> [TextBlock]
getBlock ((StartElement "TextBlock" attrs):xs) = 
    let (strings, blocks) = getString xs
    in TextBlock{ block_id = getAttribute "ID" attrs, tokens = unwords $ strings} : blocks
getBlock (EndElement "Page":xs) = [] -- Only 1 Page expected per file
getBlock (_:xs) = getBlock xs
getBlock [] = error "could not find end of <Page>" 

getString :: [SAXEvent XMLToken XMLToken] -> ([String], [TextBlock])
getString ((StartElement "String" attrs):xs) = 
    let (strings, blocks) = getString xs
    in (  (parseString attrs) : strings, blocks)
getString (EndElement "TextBlock":xs) = ([], getBlock xs)
getString (_:xs) = getString xs
getString [] = error "could not find end of <TextBlock>" 

parseString :: [(XMLToken, XMLToken)] -> String
parseString attrs = let
        content = getAttribute "CONTENT" attrs
        subsType = getAttributeOptional "SUBS_TYPE" attrs
        subsContent = getAttributeOptional "SUBS_CONTENT" attrs
        in  if (subsContent /= "" && subsType == "HypPart1") then subsContent 
            else if (subsType == "HypPart2") then "" else content

pageVertXML :: (String -> [Token]) -> Page -> String
pageVertXML analyze (Page nr blocks) = 
    if null blocks then "" else 
    "<page nr=\"" ++ nr ++ "\">\n" ++
    unlines (filter ((/=) "") (map (blockVertXML analyze) blocks)) ++
    "</page>"

pageVertXML_T :: (String -> [Token_T]) -> Page -> T.Text
pageVertXML_T analyze (Page nr blocks) = 
    if null blocks then "" else 
    "<page nr=\"" `T.append` (T.pack nr) `T.append` "\">\n" `T.append`
    (T.unlines (filter ((/=) "") (map (blockVertXML_T analyze) blocks))) `T.append`
    "</page>"

pageStanfordVert :: (String -> [Token_T]) -> Page -> T.Text
pageStanfordVert analyze (Page nr blocks) = 
    if null blocks then "" else 
    T.unlines (filter ((/=) "") (map (blockStanfordVert analyze) blocks))
    
pageTagSoupXML :: (String -> [Token]) -> Page -> String
pageTagSoupXML analyze (Page nr blocks) = "<page nr=\"" ++ nr ++ "\">\n" ++
    unlines (map (blockTagSoupXML analyze) blocks) ++
    "</page>"

pagePlaintext :: (String -> [Token]) -> Page -> String
pagePlaintext analyze (Page _ blocks) = 
-- versija kas saliek atstarpes starp tokeniem vai nesaliek
    unlines $ map (\x -> unwords $ map word (analyze $ tokens x)) blocks
--    unlines $ map tokens blocks

blockVertXML :: (String -> [Token]) -> TextBlock -> String
--blockVertXML analyze (TextBlock _ tokens) = "<p>\n" ++ (unlines $ map formatVertToken (analyze $ tokens)) ++ "</p>"
blockVertXML analyze (TextBlock _ tokens) = 
    if null tokens then "" else "<p>\n" ++ (unlines $ map formatVertToken (gTag tokens $ analyze $ tokens)) ++ "</p>"

blockVertXML_T :: (String -> [Token_T]) -> TextBlock -> T.Text
--blockVertXML analyze (TextBlock _ tokens) = "<p>\n" ++ (unlines $ map formatVertToken (analyze $ tokens)) ++ "</p>"
blockVertXML_T analyze (TextBlock _ tokens) = 
    if null tokens then "" else "<p>\n" `T.append` (T.unlines $ map formatVertToken_T (gTag_T (T.pack tokens) $ analyze $ tokens)) `T.append` "</p>"

blockStanfordVert :: (String -> [Token_T]) -> TextBlock -> T.Text
--blockStanfordVert analyze (TextBlock _ tokens) = "<p>\n" ++ (unlines $ map formatVertToken (analyze $ tokens)) ++ "</p>"
blockStanfordVert analyze (TextBlock _ tokens) = 
    if null tokens then "" else (T.unlines $ map formatStanfordVertToken (gTag_T (T.pack tokens) $ analyze $ tokens)) `T.append` "<p/>"

blockTagSoupXML :: (String -> [Token]) -> TextBlock -> String
-- versija kas saliek atstarpes starp tokeniem vai nesaliek
blockTagSoupXML analyze (TextBlock _ tokens) = "<p>" ++ (unwords $ map (xmlEscape . word) (analyze $ tokens)) ++ "</p>"
--blockTagSoupXML analyze (TextBlock _ tokens) = "<p>" ++ (xmlEscape tokens) ++ "</p>"

formatVertToken :: Token -> String
formatVertToken (Token "<g />" _ _) = "<g />"
formatVertToken (Token word lemma tag) = (xmlEscape word) ++ "\t" ++ (xmlEscape lemma) ++ "\t" ++ tag ++ "\t" ++ (xmlEscape $ map toLower word)

formatVertToken_T :: Token_T -> T.Text
formatVertToken_T (Token_T "<g />" _ _) = "<g />"
formatVertToken_T (Token_T word lemma tag) = (xmlEscape_T word) `T.append` "\t" `T.append` (xmlEscape_T lemma) `T.append` "\t" `T.append` tag `T.append` "\t" `T.append` (xmlEscape_T $ T.toLower word)

formatStanfordVertToken :: Token_T -> T.Text
formatStanfordVertToken (Token_T "<g />" _ _) = "<g/>\t-\t<g/>\t-"
formatStanfordVertToken (Token_T word lemma tag) = word `T.append` "\t" `T.append` (T.take 1 tag) `T.append` "\t" `T.append` lemma `T.append` "\t" `T.append` tag

xmlEscape :: String -> String
--xmlEscape source = replace ">" "&gt;" $ replace "<" "&lt;" $ replace "&" "&amp;" $ replace "\"" "&quot;" $ replace "'" "&apos;" $ source
xmlEscape source = replace "<" "&lt;" $ replace "\"" "&quot;" $ replace "&" "&amp;" $ source -- minimums kas obligaati jaaeskeipo

xmlEscape_T :: T.Text -> T.Text
--xmlEscape source = replace ">" "&gt;" $ replace "<" "&lt;" $ replace "&" "&amp;" $ replace "\"" "&quot;" $ replace "'" "&apos;" $ source
xmlEscape_T source = T.replace "<" "&lt;" $ T.replace "\"" "&quot;" $ T.replace "&" "&amp;" $ source -- minimums kas obligaati jaaeskeipo


gTag :: String -> [Token] -> [Token]
gTag text (t:ts) | startswith (word t) text = t : (gTagCheckSpace (drop (length $ word t) text) ts)
                 | otherwise = t : (gTag text ts) -- FIXME - shim nekad nevajadzeetu buut, bet ja nu ir tad vismaz atgrieziisim visus tokenus nesaliekot </g> 
gTag _ [] = []

gTag_T :: T.Text -> [Token_T] -> [Token_T]
gTag_T text (t:ts) | T.isPrefixOf (word_t t) text = t : (gTagCheckSpace_T (T.drop (T.length $ word_t t) text) ts)
                 | otherwise = t : (gTag_T text ts) -- FIXME - shim nekad nevajadzeetu buut, bet ja nu ir tad vismaz atgrieziisim visus tokenus nesaliekot </g> 
gTag_T _ [] = []

gTagCheckSpace :: String -> [Token] -> [Token]
gTagCheckSpace (' ':text) tokens = gTag text tokens
gTagCheckSpace "" tokens = gTag "" tokens
gTagCheckSpace text tokens = Token{word="<g />", lemma="", tag = ""} : (gTag text tokens)

gTagCheckSpace_T :: T.Text -> [Token_T] -> [Token_T]
gTagCheckSpace_T text tokens 
    | T.isPrefixOf " " text = gTag_T text tokens
    | text == "" = gTag_T "" tokens
    | otherwise = Token_T{word_t="<g />", lemma_t="", tag_t = ""} : (gTag_T text tokens)
