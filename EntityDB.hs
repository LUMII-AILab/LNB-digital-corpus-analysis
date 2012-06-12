module EntityDB
(tagNERDocument) where

import Morphology
import ParseALTO
import ParseMETS
import Data.Time.Clock
import Data.List
import Data.Char
import qualified Data.HashMap as H

tagNERDocument :: Document -> IO ()
tagNERDocument doc = do
	putStrLn $ insertDoc doc
	entities <- readEntities
	--showMap $ foldl (tagNERPage entities) H.empty $ pages doc
	mapM (putStrLn . insertLink doc) $ H.toList $ foldl (tagNERPage entities) H.empty $ pages doc
	return ()

tagNERPage :: [String] -> H.Map String Int -> Page -> H.Map String Int
tagNERPage entities nermap page = 
	foldl (tagNERText entities) nermap $ map tokens (blocks page)
	
tagNERText :: [String] -> H.Map String Int -> String -> H.Map String Int 
tagNERText entities nermap text = 
	foldl (checkNER $ map toLower text) nermap entities


checkNER :: String -> H.Map String Int -> String -> H.Map String Int
checkNER text oldmap entity = 
	if isInfixOf (map toLower entity) text then
		H.insertWith (+) entity 1 oldmap
	else
		oldmap

showMap :: H.Map String Int -> IO ()
showMap m = putStrLn $ show m


getURL :: String -> String 
getURL docId = 
	"http://www.periodika.lv/periodika2-viewer/view/index-dev.html#panel:pp|issue:/" ++ docId
	--"http://www.periodika.lv/periodika2-viewer/view/index-dev.html#panel:pp|issue:/p_001_ilzu1925n02|query:Rainis"

insertDoc :: Document -> String
insertDoc doc = 
	"insert into document (title, date, reference, type, infoSource) values (\"" ++ title doc ++ "\",\"" ++ issueDate doc ++ "\",\"" ++ getURL (docId doc)  ++ "\",\"" ++ dType doc ++ "\",\"PP imports\")"
	--insert into document (title, author, date, reference, type, infoSource) values ("title","author","date","reference","type","infosource")

insertLink :: Document -> (String, Int) -> String
insertLink doc (entity, occurrences) =
	"insert into nameDocument (nameID, documentID, occurrences, infoSource) select min(name.ID) as nameID, min(document.ID) as documentID, " ++ show occurrences ++ 
	" as occurrences, \"PP imports\" from name, document where name = \"" ++ entity ++ "\" and reference = \"" ++ getURL (docId doc) ++ "\";"

	--insert into nameDocument (nameID, documentID, occurrences, infoSource) 
--select min(name.ID) as nameID, min(document.ID) as documentID, 42 as occurrences, "test"
--from name, document where name = "AB DAMBIS" and reference = "http://www.periodika.lv/periodika2-viewer/view/index-dev.html#panel:pp|issue:/p_001_ilzu1925n02";


--- test data

--entities = ["Rainis", "Aspazija"]
readEntities = do
	f <- readFile "/Users/pet/Documents/nerDBfilter.txt"
	return $ lines f

------------ test code

main = testNERTagging

samplefolder = "/Users/pet/Documents/LNBall/24-01-01/LNB-Periodika 4/p_001_ilzu1925n02/"
metsFile = "ilzu1925n02_mets.xml"

testNERTagging = do
    start <- getCurrentTime
    (inp,out,err,pid) <- pipeInit
    d <- getMETSDocument samplefolder metsFile
    tagNERDocument d
    --putStr $ documentVertXML (pipeAnalyze inp out) d

    stop <- getCurrentTime
    putStrLn $ show $ diffUTCTime stop start
    pipeClose inp pid
    return ()