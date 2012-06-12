import System.IO
import System.Exit
import Text.XML.HXT.Core
import Text.XML.HXT.Curl

testfolder = "/Users/pet/Documents/p_001_cina1986n001/"
testfile = "cina1986n001_mets.xml"

processLNB_mets h = do
	contents <- hGetContents h
	putStr . unlines . (take 4) . lines $ contents


--main = do
--	withFile (testfolder ++ testfile) ReadMode processLNB_mets

main        :: IO ()
main
     = do
       [rc] <- runX
               ( configSysVars [ withValidate no
                               , withCurl []
                               ]
                 >>>
                 readDocument  []
                               "http://eksperimenti.ailab.lv/kamols/?analizet=meitenes"
                 >>>
                 writeDocument [ withOutputEncoding isoLatin1
                               ]
                               ""                     -- output to stdout
                 >>>
                 getErrStatus
               )
       exitWith ( if rc >= c_err
                  then ExitFailure 1
                  else ExitSuccess
                )