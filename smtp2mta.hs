module Main where

import Data.Char (toUpper)
import Network
import System.Environment (getArgs)
import System.IO
import System.Exit (ExitCode(..))
import System.Process (proc,createProcess,StdStream(CreatePipe),CreateProcess(std_in),waitForProcess)
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
	case (getCmd args, errors) of
		(Nothing, _)           -> usage errors
		(_, (_:_))             -> usage errors
		_ | Help `elem` flags  -> usage errors
		(Just (cmd,cargs), _) | InetD `elem` flags -> do
			configureHandle stdin
			configureHandle stdout
			simpleServer (lineInOut stdin stdout) cmd cargs
				`safeFinally` hClose stdout
		(Just (cmd, cargs), _) -> withSocketsDo $ do
			sock <- listenOn $ getListen flags
			forever $ do
				(h,_,_) <- accept sock
				configureHandle h
				forkIO $ simpleServer (lineInOut h h) cmd cargs
					`safeFinally` (hIsClosed h >>= (`unless` hClose h))
	where
	getCmd [] = Nothing
	getCmd (cmd:args) = Just (cmd, args)

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

runMTA :: FilePath -> [String] -> (Handle -> IO ()) -> IO ExitCode
runMTA cmd args k = do
	(Just stn, _, _, ph) <- createProcess (proc cmd args) { std_in = CreatePipe }
	k stn
	hClose stn
	waitForProcess ph

simpleServer :: LineHandle -> FilePath -> [String] -> IO ()
simpleServer h cmd cargs = do
	h (Out "220 localhost smtp2mta")
	processLines h cmd cargs Nothing []

processLines :: LineHandle -> String -> [String] -> Maybe String -> [String] -> IO ()
processLines h cmd cargs from rcpt = do
	line <- h In
	let tok = words line
	let word2 = tok !! 1
	let word3 = tok !! 2
	let word2U = map toUpper word2
	case map toUpper $ safeHead tok "" of
		("HELO") -> noop
		("EHLO") -> noop
		("NOOP") -> noop
		("MAIL") | word2U == "FROM:" -> do
			h (Out "250 OK")
			next (extractAddr word3) []
		         | otherwise -> do
			h (Out "250 OK")
			next (extractAddr $ snd $ split (/= ':') word2) []
		("RCPT") | word2U == "TO:" -> do
			h (Out "250 OK")
			next from (extractAddr word3 `maybePrepend` rcpt)
		         | otherwise -> do
			h (Out "250 OK")
			next from $ extractAddr (snd $ split (/= ':') word2)
				`maybePrepend` rcpt
		("DATA") | null rcpt -> do
			h (Out "554 no valid recipients")
			next Nothing []
		         | otherwise -> do
			h (Out "354 Send data")
			mailLines <- getMailLines h
			code <- runMTA cmd finalArgs (\i -> mapM_ (hPutStrLn i) mailLines)
			case code of
				ExitSuccess -> h (Out "250 OK")
				ExitFailure _ -> h (Out "451 command failed")
			next Nothing []
		("RSET") -> do
			h (Out "250 OK")
			next Nothing []
		("QUIT") -> h (Out "221 localhost all done") >> return ()
		_ -> do
			h (Out "500 Command unrecognized")
			next from rcpt
	where
	next = processLines h cmd cargs
	noop = h (Out "250 OK") >> next from rcpt
	maybePrepend (Just x) xs = x:xs
	maybePrepend Nothing xs = xs
	finalArgs = cargs ++ maybe [] (\f -> ["-f",f]) from ++ ["--"] ++ rcpt
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

getMailLines :: LineHandle -> IO [String]
getMailLines h = getMailData' []
	where
	getMailData' lines = do
		line <- h In
		if line == "." then return (reverse lines) else
			if safeHead line '\0' == '.' then getMailData' $ tail line:lines else
				getMailData' $ line:lines
