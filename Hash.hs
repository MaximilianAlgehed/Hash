-- important imports
import System.Console.Readline
import System.Directory
import System.Environment
import System.Posix.Process
import System.Posix.IO
import System.Posix.Files
import System.Posix.Signals
import Control.Exception
import Control.Concurrent
import Data.List.Split

-- Execute a program with a given redirect for stdin and out
executeWith s in_fd out_fd = do
                                dupTo in_fd stdInput
                                dupTo out_fd stdOutput
                                executeFile (head (splitOn " " s)) True (filter (not . null) (tail (splitOn " " s))) Nothing

-- Start the processes given in a list
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

-- Strip a set of programs of file redirects
fixPgms [] = []
fixPgms (x:xs) = (filter (\x -> x /= '&') ((head (splitOn " > " (head (splitOn " < " x)))))):(fixPgms xs) 

-- Redirect stdin to file if needed
getInput (x:_)
    | elem '<' x = openFd dropped ReadOnly (Just ownerModes) defaultFileFlags
    | otherwise = return stdInput
    where
        dropped = takeWhile (\x -> x /= ' ') $ drop 2 $ dropWhile (\x -> x /= '<') x
-- Redirect stdout to file if needed
getOutput (x:_)
    | elem '>' x = openFd dropped WriteOnly (Just ownerModes) defaultFileFlags
    | otherwise = return stdOutput
    where
        dropped = takeWhile (\x -> x /= ' ') $ drop 2 $ dropWhile (\x -> x /= '>') x

-- Start all the programs in the list pgms
startPrograms pgms background = do
                                    input <- getInput pgms
                                    output <- getOutput (reverse pgms)
                                    makeProcesses (fixPgms pgms) input output background

-- Handle all the input
handleInput "exit" = return False -- exit
handleInput ('c':'d':xs) -- cd
    | xs == "" = (getEnv "HOME") >>= setCurrentDirectory >> return True -- cd to home
    | (head xs) == ' ' = do -- cd to wherever
                            result <- try $ setCurrentDirectory (tail xs) :: IO (Either SomeException ())
                            case result of
                                Left ex -> putStrLn "Action not possible" >> return True
                                Right () -> return True
-- Any number of piped programs
handleInput s = do
                    let pgms = filter (\x -> x /= "") $ splitOn " | " s
                    processes <- (if not (null pgms) then startPrograms pgms (background s)
                                    else return [])
                    if not (background s) then waitFor processes
                        else (if not (null processes) then ((forkIO (waitFor processes)) >> return ())
                                else return ())
                    return True
                    where
                        waitFor processes = do
                                                sequence $ map (\pid -> getProcessStatus True True pid) processes
                                                return ()
                        background = elem '&'
-- Readline and execute it
readEvalLoop = do
                maybeLine <- getCurrentDirectory >>= readline . (\s->s++" >>= ")
                case maybeLine of
                    Nothing -> putStrLn "" >>= return
                    Just line -> do
                                 addHistory line
                                 exit <- handleInput line 
                                 if not exit then return ()
                                    else readEvalLoop

-- Get the ball rolling
main :: IO ()
main = do
        installHandler sigINT Ignore Nothing
        readInitFile "/etc/inputrc"
        readEvalLoop
