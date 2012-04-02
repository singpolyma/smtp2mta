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

data LineInOut = In | Out String
type LineHandle = LineInOut -> IO String

data Flag = Listen PortNumber | Help | InetD deriving (Eq)
options :: [OptDescr Flag]
options = [
		Option ['l'] ["listen"] (ReqArg port "PORT") "Listen on PORT (default 2525)",
		Option ['i'] ["inetd"] (NoArg InetD) "Handle a single connection on stdin/stdout",
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
main = do
	(flags, args, errors) <- liftM (getOpt RequireOrder options) getArgs

	if length errors > 0 || Help `elem` flags then usage errors else
		if InetD `elem` flags then do
			configureHandle stdin
			configureHandle stdout
			simpleServer (lineInOut stdin stdout) (unwords args)
				`safeFinally` hClose stdout
		else withSocketsDo $ do
			sock <- listenOn $ getListen flags
			forever $ do
				(h,_,_) <- accept sock
				configureHandle h
				forkIO $ simpleServer (lineInOut h h) (unwords args)
					`safeFinally` (hIsClosed h >>= (`unless` hClose h))
	where
	getListen [] = PortNumber 2525
	getListen (Listen p : _) = PortNumber p
	getListen (_:xs) = getListen xs

lineInOut :: Handle -> Handle -> LineInOut -> IO String
lineInOut i _ In = hGetLine i
lineInOut _ o (Out s) = hPutStrLn o s >> return ""

configureHandle :: Handle -> IO ()
configureHandle h = do
	hSetBinaryMode h False
	hSetBuffering h LineBuffering
	hSetNewlineMode h NewlineMode {inputNL = CRLF, outputNL = CRLF}

safeFinally :: IO () -> IO b -> IO ()
safeFinally x y = catch (x `finally` y) (\(SomeException _) -> return ())

safeHead :: [a] -> a -> a
safeHead [] def = def
safeHead list _ = head list

simpleServer :: LineHandle -> String -> IO ()
simpleServer h cmd = do
	h (Out "220 localhost smtp2mta")
	processLines h cmd Nothing []

processLines :: LineHandle -> String -> Maybe String -> [String] -> IO ()
processLines h cmd from rcpt = do
	line <- h In
	let tok = words line
	let word2 = tok !! 1
	let word3 = tok !! 2
	let word2U = map toUpper word2
	case map toUpper $ safeHead tok "" of
		("HELO") -> noop
		("EHLO") -> noop
		("MAIL") | word2U == "FROM:" -> do
			h (Out "250 OK")
			processLines h cmd (extractAddr word3) []
		         | otherwise -> do
			h (Out "250 OK")
			processLines h cmd (extractAddr $ snd $ split (/= ':') word2) []
		("RCPT") | word2U == "TO:" -> do
			h (Out "250 OK")
			processLines h cmd from (extractAddr word3 `maybePrepend` rcpt)
		         | otherwise -> do
			h (Out "250 OK")
			processLines h cmd from $ extractAddr (snd $ split (/= ':') word2)
				`maybePrepend` rcpt
		("DATA") ->
			if null rcpt then do
				h (Out "554 no valid recipients")
				processLines h cmd Nothing []
			else do
				h (Out "354 Send data")
				mailLines <- getMailLines h
				(i,_,_,ph) <- runInteractiveCommand finalCmd
				mapM_ (hPutStrLn i) mailLines
				hClose i
				code <- waitForProcess ph
				case code of
					ExitSuccess -> h (Out "250 OK")
					ExitFailure _ -> h (Out "451 command failed")
				processLines h cmd Nothing []
		("RSET") -> do
			h (Out "250 OK")
			processLines h cmd Nothing []
		("NOOP") -> noop
		("QUIT") -> h (Out "221 localhost all done") >> return ()
		_ -> do
			h (Out "500 Command unrecognized")
			processLines h cmd from rcpt
	where
	noop = do
		h (Out "250 OK")
		processLines h cmd from rcpt
	finalCmd = cmd ++ fromArg from ++ " -- " ++ unwords (map shellEsc rcpt)
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

getMailLines :: LineHandle -> IO [String]
getMailLines h = getMailData' []
	where
	getMailData' lines = do
		line <- h In
		if line == "." then return (reverse lines) else
			if safeHead line '\0' == '.' then getMailData' $ tail line:lines else
				getMailData' $ line:lines
