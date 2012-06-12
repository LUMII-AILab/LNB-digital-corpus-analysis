{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module ParseMETS
(getMETSDocument, documentTagSoupXML, documentTagSoupXML3, documentVertXML, documentPlaintext, documentMetadata, docTokens, stripPages, emptydoc, Document(Document), pages, docId, title, issueDate, dType, language, label) where

import Data.String.Utils
import Morphology
import ParseALTO
import Debug.Trace
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO
import System.FilePath
import Data.List
import Data.Char
import Data.Monoid
import Data.Time.Clock
import Text.XML.Expat.SAX as SAX

test1 = "Rīgas LattvMu Beedribas Derīgu Grahmatu Nodaļas isdelvums. cirvis"

main = tokenizeplaintext

testgtag = do
    (inp,out,err,pid) <- pipeInit
    let text = test1
    let tokens = (pipeAnalyze inp out) text
    putStr $ unlines $ map formatVertToken tokens
    putStrLn $ "------------------------"
    putStr $ unlines $ map formatVertToken $ gTag test1 tokens
    pipeClose inp pid
    return ()

tokenizeplaintext = do
    (inp,out,err,pid) <- pipeInit
    text <- readFile "/Users/pet/Documents/Paraugdokumenti/Pumpurs.txt"
    let tokens = map (\x -> unwords $ map word x) $ map (pipeAnalyze inp out) $ map strip $ lines text
    writeFile "/Users/pet/Documents/Paraugdokumenti/Pumpurs tokeni.txt" $ unlines tokens
    pipeClose inp pid
    return ()

samplefolder = "/Users/pet/Documents/LNBall/01-01-01/LNB-Grammata 5/g_001_0300031823/"
metsFile = "0300031823_mets.xml"
altoFile = "0300031823_005_alto.xml"

folderPrefix = "/Users/pet/Documents/LNBall/"

testALTO = do
    page <- getALTOPage (samplefolder ++ altoFile)
    putStr $ pageTagSoupXML cheapAnalyze page
    return ()

data Document = Document {docPath, docId, issueDate, title, label, dType, pagecount, language, textcode :: String, pages :: [Page], pagenames :: [FilePath]}  --TODO - possible other metainformation could be extracted from METS
    deriving (Show, Eq)

emptydoc = Document {docPath="", docId = "", issueDate = "", title = "", label = "", dType = "", pagecount = "0", language = "", textcode = "", pages = [], pagenames = []}

firstelem s = if (s == []) then "" else (head s)

testMETS = do
    start <- getCurrentTime
    (inp,out,err,pid) <- pipeInit
    d <- getMETSDocument samplefolder metsFile
    --B.putStr $ toByteString $ documentTagSoupXML2 d
    putStr $ documentVertXML (pipeAnalyze inp out) d
    --TIO.putStr $ documentTagSoupXML3 d
    stop <- getCurrentTime
    putStrLn $ show $ diffUTCTime stop start
    pipeClose inp pid
    return ()

unpack :: XMLToken -> String
unpack x = BC.toString x

type DocParser = [SAXEvent XMLToken XMLToken] -> Document
--type ValueParser = [SAXEvent XMLToken XMLToken] -> XMLToken

getMETSDocument :: String -> String -> IO Document
getMETSDocument folder filename = do
    docText <- B.readFile (folder ++ filename)
    document <- processPages $ metsStart folder (SAX.parse defaultParseOptions $ BSL.fromChunks [docText]) 
    let docname = folder ++ takeWhile (/= '.') filename
    return $ postProcess $ setId document (replace folderPrefix "" docname)

processPages :: Document -> IO Document
processPages doc = do
    pages <- mapM getALTOPage $ pagenames doc
    return doc{pages = pages}
    --return doc{pages = []}   Priekš metadatu ģenerēšanas

postProcess :: Document -> Document
postProcess doc = 
    let oldDate = take 8 $ issueDate doc
    in doc{ issueDate = oldDate ++ take (8 - length oldDate) (repeat '0')}

stripPages :: Document -> Document
stripPages doc = doc {pages = []} 

metsStart :: String -> DocParser
metsStart folder ((StartElement "mets:mets" attrs):xs) = getHeading attrs (metadataStart folder xs)
metsStart folder (_:xs) = metsStart folder xs
metsStart _ [] = error "could not find <mets:mets>"

getHeading :: [(XMLToken, XMLToken)] -> Document -> Document
getHeading attrs doc = let
    label = getAttribute "LABEL" attrs
    dType = getAttribute "TYPE" attrs
    in doc {label = label, dType = dType}

metadataStart :: String -> DocParser
metadataStart folder ((StartElement "mets:dmdSec" attrs):xs) = (if metadataValid attrs then getMetadata folder else metadataStart folder) xs
metadataStart folder (_:xs) = metadataStart folder xs
metadataStart _[] = error "could not find <mets:dmdSec>"

--TODO - principā tā dokumenta vilkšana līdzi pēc būtības laikam tak ir Arrow... jāpalasa un jāsaprot
getMetadata :: String -> DocParser
getMetadata folder ((StartElement "mods:title" _):xs) = getTitle (getMetadata folder) xs
getMetadata folder ((StartElement "mods:dateIssued" _):xs) = getDateIssued (getMetadata folder) xs
getMetadata folder ((StartElement "mods:detail" attrs):xs) = if (getAttribute "type" attrs) == "pages" then getPageCount (getMetadata folder) xs else (getMetadata folder) xs
getMetadata folder ((StartElement "mods:languageTerm" attrs):xs) = 
    if (getAttribute "type" attrs) == "code" then getLanguage (getMetadata folder) xs else 
    if (getAttribute "type" attrs) == "text" then getTextCode (getMetadata folder) xs else getMetadata folder xs
getMetadata folder ((EndElement "mets:dmdSec"):xs) = getPageFiles folder xs  --FIXME - pienjeemums ka page apraksts seko peec metadatiem, kas var nebuut true
getMetadata folder (_:xs) = getMetadata folder xs
getMetadata _ [] = error "could not find end of <mets:dmdSec>"

--FIXME - daudz duplikaacijas ko vareetu iznest kautkaa aaraa
getTitle :: DocParser -> DocParser
getTitle cont ((CharacterData txt):xs ) = (getTitle cont xs){title = unpack txt}
getTitle cont ((EndElement "mods:title"):xs ) = cont xs
getTitle cont (_:xs) = getTitle cont xs
getTitle _ [] = error "problem with title tag"

getDateIssued :: DocParser -> DocParser
getDateIssued cont ((CharacterData txt):xs ) = (getDateIssued cont xs){issueDate = unpack txt}
getDateIssued cont ((EndElement "mods:dateIssued"):xs ) = cont xs
getDateIssued cont (_:xs) = getDateIssued cont xs
getDateIssued _ [] = error "problem with dateIssued tag"

getPageCount :: DocParser -> DocParser
getPageCount cont ((StartElement "mods:partNumber" _):xs) = getPageText cont xs
getPageCount cont ((EndElement "mods:detail"):xs ) = cont xs
getPageCount cont (_:xs) = getPageCount cont xs
getPageCount _ [] = error "problem with detail/partnumber tag"

getPageText :: DocParser -> DocParser
getPageText cont ((CharacterData txt):xs ) = (getPageText cont xs){pagecount = unpack txt}
getPageText cont ((EndElement "mods:partNumber"):xs ) = cont xs
getPageText cont (_:xs) = getPageText cont xs
getPageText _ [] = error "page number not found"

getLanguage :: DocParser -> DocParser
getLanguage cont ((CharacterData txt):xs ) = (getLanguage cont xs){language = unpack txt}
getLanguage cont ((EndElement "mods:languageTerm"):xs ) = cont xs
getLanguage cont (_:xs) = getLanguage cont xs
getLanguage _ [] = error "problem with language tag"

getTextCode :: DocParser -> DocParser
getTextCode cont ((CharacterData txt):xs ) = (getTextCode cont xs){textcode = unpack txt}
getTextCode cont ((EndElement "mods:languageTerm"):xs ) = cont xs
getTextCode cont (_:xs) = getTextCode cont xs
getTextCode _ [] = error "problem with language tag"

metadataValid :: [(XMLToken, XMLToken)] -> Bool
metadataValid attrs = let
    sec_id = getAttribute "ID" attrs
    in (sec_id == "modsissue" || sec_id == "modsbook")

getPageFiles :: String -> DocParser
getPageFiles folder ((StartElement "mets:fileGrp" attrs):xs) = if (getAttribute "ID" attrs) == "ALTOGRP" then getPage folder xs else getPageFiles folder xs
getPageFiles folder (_:xs) = getPageFiles folder xs
getPageFiles _ [] = error "could not find <mets:fileGrp ID=\"ALTOGRP\">"

getPage :: String -> DocParser
getPage folder ((StartElement "mets:FLocat" attrs):xs) = let
    location = getAttribute "xlink:href" attrs
    filename = folder ++ (drop 7 location) -- expecting 'file://' at start
    document = getPage folder xs
    prevPages = pagenames document
    in document{ pagenames = filename : prevPages}
getPage folder ((EndElement "mets:fileGrp"):xs) = emptydoc -- TODO - pienjeemums ka aiz lapu datiem vairs nekas svariigs neseko
getPage folder (_:xs) = getPage folder xs
getPage _ [] = error "could not find end of <mets:fileGrp ID=\"ALTOGRP\">"

setId :: Document -> String -> Document
setId doc docpath = doc {docPath = docpath, docId = getFolderName docpath}

getFolderName :: String -> String
getFolderName docpath = 
    let items = split [pathSeparator] docpath in
        if length items >= 2 then last $ init $ items else ""

debugtext :: String -> IO String
debugtext text = do
    putStrLn text
    hFlush stdout
    return text

documentMetadata :: Document -> String
documentMetadata (Document docPath docId issueDate title label dType pagecount language textcode pages pagenames) = 
    docId ++ "\t" ++ docPath ++ "\t" ++ issueDate ++ "\t" ++ title ++ "\t" ++ label ++ "\t" ++ dType ++ "\t" ++ pagecount ++ "\t" ++ (show $ length pagenames) ++ "\t" ++ language ++ "\t" ++ textcode

documentVertXML :: (String -> [Token]) -> Document -> String
documentVertXML analyze (Document docPath docId issueDate title label dType pagecount language textcode pages _) = 
    "<doc id=\"" ++ docId ++ "\" " ++
    "path=\"" ++ docPath ++ "\" " ++
    "date=\"" ++ issueDate ++ "\" " ++
    "title=\"" ++ title ++ "\" " ++
    "label=\"" ++ label ++ "\" " ++
    "label_lowercase=\"" ++ map toLower label ++ "\" " ++
    "type=\"" ++ dType ++ "\" "++
    "pages=\"" ++ pagecount ++ "\" "++
    "language=\"" ++ language ++ "\" " ++
    "textcode=\"" ++ textcode ++ "\">\n" ++
    unlines (map (pageVertXML (if language == "lav" then analyze else nonLVAnalyze)) pages) ++
    "</doc>\n"

documentTagSoupXML :: (String -> [Token]) -> Document -> String
documentTagSoupXML analyze (Document docPath docId issueDate title label dType pagecount language textcode pages _) =
    "<doc id=\"" ++ docId ++ "\" " ++
    "path=\"" ++ docPath ++ "\" " ++
    "date=\"" ++ issueDate ++ "\" " ++
    "title=\"" ++ title ++ "\" " ++
    "label=\"" ++ label ++ "\" " ++
    "type=\"" ++ dType ++ "\" " ++
    "pages=\"" ++ pagecount ++ "\" " ++
    "realpages=\"" ++ (show $ length pages) ++ "\" " ++
    "language=\"" ++ language ++ "\" " ++
    "textcode=\"" ++ textcode ++ "\">\n" ++
    unlines (map (pageTagSoupXML analyze) pages) ++
    "</doc>\n"

documentTagSoupXML3 :: (String -> [Token]) -> Document -> T.Text
documentTagSoupXML3 analyze (Document docPath docId issueDate title label dType pagecount language textcode pages _) =
    (T.pack ("<doc id=\"" ++ docId ++ "\" " ++
    "path=\"" ++ docPath ++ "\" " ++
    "date=\"" ++ issueDate ++ "\" " ++
    "title=\"" ++ title ++ "\" " ++
    "label=\"" ++ label ++ "\" " ++
    "type=\"" ++ dType ++ "\" " ++
    "pages=\"" ++ pagecount ++ "\" " ++
    "realpages=\"" ++ (show $ length pages) ++ "\" " ++
    "language=\"" ++ language ++ "\" " ++
    "textcode=\"" ++ textcode ++ "\">\n")) `T.append` 
    (T.unlines $ map (T.pack . (pageTagSoupXML analyze)) pages) `T.append`
    (T.pack "</doc>\n")

documentPlaintext :: (String -> [Token]) -> Document -> String
documentPlaintext analyze (Document _ _ _ _ _ _ _ _ _ pages _) =
    unlines $ map (pagePlaintext analyze) pages

docTokens :: Document -> Int
docTokens doc = sum $ map pageTokens $ pages doc

pageTokens :: Page -> Int
pageTokens (Page _ blocks) = sum $ map blockTokens blocks

blockTokens :: TextBlock -> Int
blockTokens (TextBlock _ tokens) = length $ words tokens
