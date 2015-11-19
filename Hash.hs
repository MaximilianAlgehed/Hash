-- important imports
import System.Console.Readline
import System.Directory
import System.Environment
import System.Posix.Process
import System.Posix.IO
import System.Posix.Files
import System.Posix.Signals
import System.Posix.Types
import Control.Exception
import Control.Concurrent
import Data.List.Split
import qualified Text.Parsec as Parsec
import qualified Parser as Parser

-- Execute a program with a given redirect for stdin and out
executeWith :: [String] -> Fd -> Fd -> IO ()
executeWith pgm in_fd out_fd = do
                                dupTo in_fd stdInput
                                dupTo out_fd stdOutput
                                executeFile (head pgm) True (filter (not . null) (tail pgm)) Nothing

-- Start the processes given in a list
makeProcess :: [String] -> Fd -> Fd -> Bool -> IO [ProcessID]
makeProcess [] _ _ _ = return []
-- We are at the end of the line, don't pipe
makeProcesses [x] in_fd out_fd background = do 
                                    pid <- forkProcess (executeWith x in_fd out_fd)
                                    if background then setProcessGroupIDOf pid 0
                                        else return ()
                                    return [pid]
-- Start the process and pipe
makeProcesses (x:xs) in_fd out_fd background = do
                                        (r, w) <- createPipe
                                        pid <- forkProcess $ executeWith x in_fd w
                                        closeFd w
                                        if background then setProcessGroupIDOf pid 0
                                            else return ()
                                        lst <- makeProcesses xs r out_fd background
                                        closeFd r
                                        return (pid:lst)

-- Redirect stdin to file if needed
getInput :: String -> IO Fd
getInput "" = return stdInput
getInput x = openFd x ReadOnly (Just ownerModes) defaultFileFlags

-- Redirect stdout to file if needed
getOutput :: String -> IO Fd
getOutput "" = return stdOutput
getOutput x = openFd x WriteOnly (Just ownerModes) defaultFileFlags

-- Any number of piped programs
handleInput :: Parser.Expression -> IO Bool
handleInput (Parser.Exit) = return False
handleInput (Parser.CD "") = (getEnv "HOME") >>= setCurrentDirectory >> return True
handleInput (Parser.CD dir) = do
                                result <- try (setCurrentDirectory dir) :: IO (Either SomeException ())
                                case result of
                                    otherwise -> return True 
handleInput (Parser.Expression inp outp pgms background) = do
                    input <- getInput inp
                    output <- getOutput outp
                    processes <- (if not (null pgms) then makeProcesses pgms input output background
                                    else return [])
                    if not background then waitFor processes
                        else (if not (null processes) then ((forkIO (waitFor processes)) >> return ())
                                else return ())
                    return True
                    where
                        waitFor processes = do
                                                sequence $ map (\pid -> getProcessStatus True True pid) processes
                                                return ()

--Execute a line
executeLine :: String -> IO Bool
executeLine line = do
                     addHistory line
                     let parsed = Parsec.parse Parser.parser "(source)" line
                     exit <- case parsed of
                                    Right [[]] -> return True
                                    Right v -> do
                                                exit <- handleInput (Parser.makeExpr v)
                                                return exit
                                    otherwise -> return True 
                     return exit 

-- Readline and execute it
readEvalLoop :: IO ()
readEvalLoop = do
                maybeLine <- getCurrentDirectory >>= readline . (\s->s++" >>= ")
                let maybeLines = fmap (splitOn ";") maybeLine
                exit <- case maybeLines of
                    Nothing -> putStrLn "" >> return False
                    Just lines ->fmap last $ sequence $ map executeLine lines
                if not exit then return ()
                    else readEvalLoop
                
-- Get the ball rolling
main :: IO ()
main = do
        installHandler sigINT Ignore Nothing
        readInitFile "/etc/inputrc"
        readEvalLoop
