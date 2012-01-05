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

	processLines h Nothing [] `safeFinally` do
		closed <- hIsClosed h
		unless closed $ hClose h

processLines :: Handle -> Maybe String -> [String] -> IO ()
processLines h from rcpt = do
	line <- hGetLine h
	let tok = words line
	let word2 = tok !! 1
	let word3 = tok !! 2
	let word2U = map toUpper word2
	case map toUpper $ head tok of
		("HELO") -> hPutStrLn h "250 OK"
		("EHLO") -> hPutStrLn h "250 OK"
		("MAIL") | word2U == "FROM:" -> do
			hPutStrLn h "250 OK"
			processLines h (extractAddr word3) []
		         | otherwise -> do
			hPutStrLn h "250 OK"
			processLines h (extractAddr $ snd $ split (/= ':') word2) []
		("RCPT") | word2U == "TO:" -> do
			hPutStrLn h "250 OK"
			processLines h from (extractAddr word3 `maybePrepend` rcpt)
		         | otherwise -> do
			hPutStrLn h "250 OK"
			processLines h from $ extractAddr (snd $ split (/= ':') word2)
				`maybePrepend` rcpt
		("DATA") -> error "TODO"
		("RSET") -> do
			hPutStrLn h "250 OK"
			processLines h Nothing []
		("QUIT") -> do
			hPutStrLn h "221 localhost all done"
			hClose h
		_ -> hPutStrLn h "500 Command unrecognized"
	processLines h from rcpt
	where
	maybePrepend (Just x) xs = x:xs
	maybePrepend Nothing xs = xs
	extractAddr s =
		let (a,b) = split (/= '<') s in
			if null b then
				if null a then Nothing else Just a
			else
				let (addr,_) = split (/= '>') b in
					if null addr then Nothing else Just addr
	split p s =
		let (a,b) = span p s in
			(a, drop 1 b)
