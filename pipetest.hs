import System.Process
import System.IO
import Control.Concurrent
import Codec.Binary.UTF8.String

pipetool = "/Users/pet/Documents/java/Morphology/pipetool.sh"
testdata = ["šķūnis","kāja","roku", "cirvis"]

main = do
	(inp,out,err,pid) <- runInteractiveCommand pipetool 
	hSetBinaryMode inp False
	hSetBinaryMode out False
	hSetBuffering inp LineBuffering
	hSetBuffering out LineBuffering
	line <- hGetLine out -- 2 header lines 'loading lexicon'
	line <- hGetLine out
	hPutStrLn inp ""
	line <- catch (hGetLine out) (\_ -> return "haha1")
	terminateProcess pid
	hFlush stdout
	--results <- mapM (darboties inp out) testdata
	--mapM putStrLn testdata
	--mapM putStrLn results
	--hPutStrLn inp ""
	--terminateProcess pid

darboties :: Handle -> Handle -> String -> IO String
darboties inp out word = do
	hPutStrLn inp word
	hGetLine out
