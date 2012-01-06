module Main where

import Data.Char (toUpper)
import Network
import System (getArgs)
import System.IO
import System.Exit (ExitCode(..))
import System.Process (runInteractiveCommand,waitForProcess)
import System.Environment (getProgName)
import System.Console.GetOpt
import Control.Monad (liftM,forever,unless)
import Prelude hiding (catch)
import Control.Exception (finally,catch,SomeException(..))
import Control.Concurrent (forkIO)

data Flag = Listen PortNumber | Help deriving (Eq)
options :: [OptDescr Flag]
options = [
		Option ['l'] ["listen"] (ReqArg port "PORT") "Listen on PORT (default 2525)",
		Option ['h'] ["help"] (NoArg Help) "Display this help message"
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

	if length errors > 0 then usage errors else
		if Help `elem` flags then usage [] else do
			sock <- listenOn $ getListen flags
			forever $ do
				(h,_,_) <- accept sock
				forkIO $ simpleServer h (unwords args)
	where
	getListen [] = PortNumber 2525
	getListen (Listen p : _) = PortNumber p
	getListen (_:xs) = getListen xs

safeFinally :: IO () -> IO b -> IO ()
safeFinally x y = catch (x `finally` y) (\(SomeException _) -> return ())

safeHead :: [a] -> a -> a
safeHead [] def = def
safeHead list _ = head list

simpleServer :: Handle -> String -> IO ()
simpleServer h cmd = do
	hSetBinaryMode h False
	hSetBuffering h LineBuffering
	hSetNewlineMode h NewlineMode {inputNL = CRLF, outputNL = CRLF}

	hPutStrLn h "220 localhost smtp2mta"

	processLines h cmd Nothing [] `safeFinally`
		(hIsClosed h >>= (`unless` hClose h))

processLines :: Handle -> String -> Maybe String -> [String] -> IO ()
processLines h cmd from rcpt = do
	line <- hGetLine h
	let tok = words line
	let word2 = tok !! 1
	let word3 = tok !! 2
	let word2U = map toUpper word2
	case map toUpper $ safeHead tok "" of
		("HELO") -> hPutStrLn h "250 OK"
		("EHLO") -> hPutStrLn h "250 OK"
		("MAIL") | word2U == "FROM:" -> do
			hPutStrLn h "250 OK"
			processLines h cmd (extractAddr word3) []
		         | otherwise -> do
			hPutStrLn h "250 OK"
			processLines h cmd (extractAddr $ snd $ split (/= ':') word2) []
		("RCPT") | word2U == "TO:" -> do
			hPutStrLn h "250 OK"
			processLines h cmd from (extractAddr word3 `maybePrepend` rcpt)
		         | otherwise -> do
			hPutStrLn h "250 OK"
			processLines h cmd from $ extractAddr (snd $ split (/= ':') word2)
				`maybePrepend` rcpt
		("DATA") ->
			if null rcpt then do
				hPutStrLn h "554 no valid recipients"
				processLines h cmd Nothing []
			else do
				hPutStrLn h "354 Send data"
				mailLines <- getMailLines h
				(i,_,_,ph) <- runInteractiveCommand finalCmd
				mapM_ (hPutStrLn i) mailLines
				hClose i
				code <- waitForProcess ph
				case code of
					ExitSuccess -> hPutStrLn h "250 OK"
					ExitFailure _ -> hPutStrLn h "451 command failed"
				processLines h cmd Nothing []
		("RSET") -> do
			hPutStrLn h "250 OK"
			processLines h cmd Nothing []
		("NOOP") -> hPutStrLn h "250 OK"
		("QUIT") -> do
			hPutStrLn h "221 localhost all done"
			hClose h
		_ -> hPutStrLn h "500 Command unrecognized"
	processLines h cmd from rcpt
	where
	finalCmd = cmd ++ fromArg from ++ " -- " ++ concatMap shellEsc rcpt
	fromArg (Just f) = " -f " ++ shellEsc f ++ " "
	fromArg Nothing = ""
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
	shellEsc s = '\'' : foldr shellEscSingle "" s ++ "\'"
	shellEscSingle '\'' s = '\\' : ('\'' : s)
	shellEscSingle c s = c : s

getMailLines :: Handle -> IO [String]
getMailLines h = getMailData' []
	where
	getMailData' lines = do
		line <- hGetLine h
		if line == "." then return (reverse lines) else
			if safeHead line '\0' == '.' then getMailData' $ tail line:lines else
				getMailData' $ line:lines
