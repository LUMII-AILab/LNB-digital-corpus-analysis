{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Control.Exception
import qualified Control.Monad.Parallel
import Morphology(Token(Token))
import System.Directory
import System.FilePath
import Data.List
import Data.String.Utils
import ParseMETS
import EntityDB
import Data.Time.Clock
import System.IO
import qualified Data.ByteString as B
import qualified Morphology

metsfolder = "/Users/pet/Documents/LNBall"
textfolder = "/Users/pet/Documents/LNB_converted"
filelist = "/Users/pet/Documents/LNBall/metsfilelist.txt"
--metsfolder = "C:\\Projects\\LNB\\paraugi"
--textfolder = "C:\\Projects\\LNB\\output"
--filelist = "C:\\Projects\\LNB\\metsfilelist.txt"

data Task = ListFiles | ProcessFiles deriving (Show, Eq)
data FileFormat = Metadata | VertXML | TagSoupXML | Plaintext | NERinfo deriving (Show, Eq)

outputFormat :: FileFormat
outputFormat = VertXML  -- config

task :: Task
task = ProcessFiles

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return $ filter (isSuffixOf "_mets.xml") $ map (replace (metsfolder++"/") "") $ concat paths

main = Control.Exception.catch ( 
  do
  start <- getCurrentTime
  case task of 
    ListFiles -> do
      paths <- getRecursiveContents metsfolder
      mapM putStrLn paths
      writeFile filelist $ unlines paths
    ProcessFiles -> do
      paths <- readFile filelist
      --runprocessing 1 ["22-01-01/LNB_Gramata_18/g_001_0307085253/0307085253_mets.xml","57-01-01/LNB_Gramata-7/g_001_0309057058/0309057058_mets.xml"] -- Maija un Paija
      --runprocessing 4 ["24-01-01/LNB-Periodika 4/p_001_ilzu1925n01/ilzu1925n01_mets.xml","24-01-01/LNB-Periodika 4/p_001_ilzu1925n02/ilzu1925n02_mets.xml","24-01-01/LNB-Periodika 4/p_001_ilzu1925n03/ilzu1925n03_mets.xml","24-01-01/LNB-Periodika 4/p_001_ilzu1925n04/ilzu1925n04_mets.xml","24-01-01/LNB-Periodika 4/p_001_ilzu1925n05/ilzu1925n05_mets.xml", "24-01-01/LNB-Periodika 4/p_001_ilzu1925n06/ilzu1925n06_mets.xml","24-01-01/LNB-Periodika 4/p_001_ilzu1925n07-08/ilzu1925n07-08_mets.xml","24-01-01/LNB-Periodika 4/p_001_ilzu1925n09/ilzu1925n09_mets.xml","24-01-01/LNB-Periodika 4/p_001_ilzu1925n10/ilzu1925n10_mets.xml", "24-01-01/LNB-Periodika 4/p_001_ilzu1925n11/ilzu1925n11_mets.xml","24-01-01/LNB-Periodika 4/p_001_ilzu1925n12/ilzu1925n12_mets.xml"]
      --runprocessing 1 $ convertnames ["/Users/pet/Documents/LNBall/24-01-01/LNB-Periodika 4/p_001_ilzu1925n02/ilzu1925n02_mets"]
      --runprocessing 1 $ take 10 $ drop 1595 $ lines paths  
      runprocessing 1 $ take 1 $ lines paths  
      --runprocessing 1 $ lines paths  
      --runprocessing 3 $ take 40 $ drop 89872 $ lines paths  
      --Performance test komplekts, kaupeena 40 burtniicinjas
  stop <- getCurrentTime
  putStrLn $ show $ diffUTCTime stop start
  hFlush stdout
  ) (\(e :: SomeException) -> do 
    putStrLn $ "Error " ++ (show e)
    hFlush stdout
    return ())

convertnames :: [String] -> [String]
convertnames = map (\x -> (replace "/Users/pet/Documents/LNBall/" "" x) ++ ".xml")

runprocessing :: Int -> [FilePath] -> IO ()
runprocessing threads paths = do
  Control.Monad.Parallel.mapM (if outputFormat /= Metadata then processfiles else processfiles_metadata) $ chunk ((((length paths) -1)`div` threads)+1) paths
  return ()
 
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = y1 : chunk n y2
  where
    (y1, y2) = splitAt n xs

processfiles :: [FilePath] -> IO ()
processfiles paths = do
  (inp,out,err,pid) <- Morphology.pipeInit
  mapM (processfile $ Morphology.pipeAnalyze inp out) paths
  Control.Exception.catch (Morphology.pipeClose inp pid) (\(e :: SomeException) -> putStrLn "nesanaaca aizveert")
  return ()

processfiles_metadata :: [FilePath] -> IO ()
processfiles_metadata paths = do
  docs <- mapM getDocHead paths
  write_metadata "index.txt" docs
--data DocCategory = NonLV | Book | PreWar | Independence | USSR | PostUSSR deriving (Show, Eq)
  write_metadata_group "nonLV.txt" NonLV docs
  write_metadata_group "books.txt" Book docs
  write_metadata_group "preWar.txt" PreWar docs
  write_metadata_group "independence.txt" Independence docs
  write_metadata_group "USSR.txt" USSR docs
  write_metadata_group "postUSSR.txt" PostUSSR docs
  return ()

write_metadata_group :: String -> DocCategory -> [Document] -> IO ()
write_metadata_group filename category docs = write_metadata filename $ filter (\x-> docCategory x == category) $ sortBy compareDoc docs

write_metadata :: String -> [Document] -> IO ()
write_metadata filename docs = do
  h <- openFile (textfolder ++ [pathSeparator] ++ filename) WriteMode
  hSetEncoding h utf8
  hPutStrLn h "filename\tissueDate\ttitle\tlabel\tdocType\tpagecount\trealpagecount\tlanguage\ttextcode"
  mapM (hPutStrLn h . documentMetadata) docs
  hClose h
  return ()

processfile :: (String -> [Token]) -> FilePath -> IO ()
processfile analyze metsfilename = Control.Exception.catch ( 
  do
  let folder = metsfolder ++ [pathSeparator] ++ (takeDirectory metsfilename) ++ [pathSeparator] 
  let filename = takeFileName metsfilename
  let resultfolder = replace metsfolder textfolder folder
  doc <- getMETSDocument folder filename
  createDirectoryIfMissing True resultfolder
  let textfilename = resultfolder ++ replace "_mets.xml" ".txt" filename
  let xmlfilename = resultfolder ++ replace "_mets.xml" "_text.xml" filename
  let vertfilename = resultfolder ++ replace "_mets.xml" "_vert.xml" filename
  case outputFormat of
    Plaintext -> writeFile textfilename $ documentPlaintext analyze doc
    TagSoupXML -> writeFile xmlfilename $ documentTagSoupXML analyze doc
    VertXML -> writeFile vertfilename $ documentVertXML analyze doc
    NERinfo -> tagNERDocument doc
    Metadata -> error "Shouldn't be here"
  hFlush stdout
  return ()
  ) (\(e :: SomeException) -> do 
    putStrLn $ "Error at " ++ metsfilename ++ (show e)
    return ())

getDocHead :: FilePath -> IO Document
getDocHead metsfilename = Control.Exception.catch ( do
  let folder = (takeDirectory metsfilename) ++ [pathSeparator] 
  let filename = takeFileName metsfilename
  doc <- getMETSDocument folder filename
  return $ stripPages doc
  ) (\(e :: SomeException) -> do 
    putStrLn $ "Error at " ++ metsfilename ++ (show e)
    return emptydoc)

data DocCategory = NonLV | Book | PreWar | Independence | USSR | PostUSSR deriving (Show, Eq)

docCategory :: Document -> DocCategory
docCategory doc =
  if language doc /= "lav" then NonLV else
    if dType doc == "METAe_Monograph" then Book else
      if issueDate doc < "19180000" then PreWar else
        if issueDate doc < "19400000" then Independence else
          if issueDate doc < "19910000" then USSR else
            PostUSSR

compareDoc :: Document -> Document -> Ordering
compareDoc d1 d2 = 
  let dates = issueDate d1 `compare` issueDate d2
  in if dates == EQ then label d1 `compare` label d2 else dates
