module Main where

import Data.Char (toUpper)
import Network
import System (getArgs)
import System.IO
import System.Environment (getProgName)
import System.Console.GetOpt
import Control.Monad (liftM,forever,unless)
import Prelude hiding (catch)
import Control.Exception (finally, catch, SomeException(..))
import Control.Concurrent (forkIO)

data Flag = Listen PortNumber
options :: [OptDescr Flag]
options = [
		Option ['l'] ["listen"] (ReqArg port "PORT") "Listen on PORT (default 2525)"
	]
	where
	port = Listen . fromIntegral . (read :: String -> Int)

usage :: [String] -> IO ()
usage errors = do
	mapM_ putStr errors
	cmd <- getProgName
	putStrLn $ usageInfo ("Usage: " ++ cmd ++ " [-l PORT] command") options

main :: IO ()
main = withSocketsDo $ do
	(flags, args, errors) <- liftM (getOpt RequireOrder options) getArgs

	if length errors > 0 then usage errors else do
		sock <- listenOn $ getListen flags
		forever $ do
			(h,_,_) <- accept sock
			forkIO $ simpleServer h
	where
	getListen [] = PortNumber 2525
	getListen (Listen p : _) = PortNumber p
	-- NOTE: Need to uncomment the following if we add other flags
	-- getListen (_:xs) = getListen xs

safeFinally :: IO () -> IO b -> IO ()
safeFinally x y = catch (x `finally` y) (\(SomeException _) -> return ())

simpleServer :: Handle -> IO ()
simpleServer h = do
	hSetBinaryMode h False
	hSetBuffering h LineBuffering
	hSetNewlineMode h NewlineMode {inputNL = CRLF, outputNL = CRLF}

	hPutStrLn h "220 localhost smtp2mta"

	forever $ do
		line <- hGetLine h
		case map toUpper $ head $ words line of
			("HELO") -> hPutStrLn h "250 OK"
			("EHLO") -> hPutStrLn h "250 OK"
			("QUIT") -> hClose h
			_ -> hPutStrLn h "500 Command unrecognized"
	`safeFinally` do
		closed <- hIsClosed h
		unless closed $ hClose h
