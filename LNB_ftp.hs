module LNB_FTP where

import Network.FTP.Client
import Network.FTP.Client.Parser
import Network.Curl 
import Codec.Binary.UTF8.String
import System.IO
import System.Directory
import System.IO.Binary
import Data.List
import Data.List.Utils


{-| uses an FTP connection to crawl the site starting from the current working directory.
    FTPConnection - connection to be used
-}
crawlCurrentFolder :: FTPConnection -> String -> IO()
crawlCurrentFolder h localpath = do
    filenames <- retrynlst h 
    mapM_ (crawlFolder h localpath) $ filter isFolder (seqList filenames)
    mapM_ (downloadFile h localpath) $ filter shouldProcess (seqList filenames)
    
crawlFolder :: FTPConnection -> String -> String -> IO()
crawlFolder h localpath foldername = do
    let newpath = localpath ++ foldername ++ "/"
    exists <- doesDirectoryExist newpath
    if exists 
        then putStrLn $ "Exists, skipping " ++ newpath
        else do
            putStrLn $ "Crawling " ++ newpath
            hFlush stdout
            cwd h foldername
            createDirectory newpath
            crawlCurrentFolder h newpath
            cwd h ".."
            return ()
    return ()

isFolder :: String -> Bool 
isFolder name =
    (not $ isInfixOf "." name) && (not $ isInfixOf "backup" name) && (not $ isInfixOf "failedValidation" name) && (not $ isInfixOf "temp" name)

shouldProcess :: String -> Bool
shouldProcess name =
    isSuffixOf "_alto.xml" name || isSuffixOf "_mets.xml" name
    
downloadFile :: FTPConnection -> String -> String -> IO()
downloadFile h localpath name = do 
    r <- retrygetbinary h name
    writeBinaryFile (localpath ++ name) r
    return ()

retrygetbinary :: FTPConnection -> String -> IO (String)
retrygetbinary h name = do
    (contents, result) <- getbinary h name
    if resultok result then return contents
        else do 
            putStrLn $ "Result " ++ (unlines $ snd result)
            putStrLn $ "Retrying file " ++ name
            retrygetbinary h name

retrynlst :: FTPConnection -> IO ([String])
retrynlst h = do
    (contents, result) <- retrlines h "NLST"
    if resultok result then return contents
        else do 
            putStrLn $ "Result " ++ (unlines $ snd result)
            putStrLn $ "Retrying NLST"
            retrynlst h

resultok :: FTPResult -> Bool
resultok (code, _) = (code >= 200) && (code < 300)

connect :: IO FTPConnection
connect = do
    h <- easyConnectFTP "terbata.lnb.lv"
    login h "lumii" (Just "LatMII-29") Nothing
    return h

testFTP = do
    h <- connect
    let startfolder = ""
    cwd h startfolder
    crawlCurrentFolder h ("/Users/pet/Documents/LNBarchive/" ++ startfolder ++ "/")
    quit h
    
main = testFTP