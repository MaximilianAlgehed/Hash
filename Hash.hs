-- important imports
import System.Console.Readline
import System.Directory
import System.Environment
import System.Posix.Process
import System.Posix.IO
import System.Posix.Files
import Data.List.Split
import Control.Exception

-- Execute a program with a given redirect for stdin and out
executeWith s in_fd out_fd = do
                                dupTo in_fd stdInput
                                dupTo out_fd stdOutput
                                executeFile (head (splitOn " " s)) True (filter (not . null) (tail (splitOn " " s))) Nothing

-- Start the processes given in a list
makeProcess [] _ _ = return []
-- We are at the end of the line, don't pipe
makeProcesses [x] in_fd out_fd = do 
                                    pid <- forkProcess (executeWith x in_fd out_fd)
                                    return [pid]
-- Start the process and pipe
makeProcesses (x:xs) in_fd out_fd = do
                                        (r, w) <- createPipe
                                        pid <- forkProcess $ executeWith x in_fd w
                                        closeFd w
                                        lst <- makeProcesses xs r out_fd
                                        closeFd r
                                        return (pid:lst)

-- Strip a set of programs of file redirects
fixPgms [] = []
fixPgms (x:xs) = (head (splitOn " > " (head (splitOn " < " x)))):(fixPgms xs) 

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
startPrograms pgms = do
                        input <- getInput pgms
                        output <- getOutput (reverse pgms)
                        makeProcesses (fixPgms pgms) input output

-- Handle all the input
handleInput "exit" = return False -- exit
handleInput ('c':'d':xs) -- cd
    | xs == "" = (getEnv "HOME") >>= setCurrentDirectory >>= (\_ -> return True) -- cd to home
    | (head xs) == ' ' = do -- cd to wherever
                            result <- try $ setCurrentDirectory (tail xs) :: IO (Either SomeException ())
                            case result of
                                Left ex -> putStrLn "Action not possible" >>= (\_ -> return True)
                                Right () -> return True
-- Any number of piped programs
handleInput s = do
                    let pgms = splitOn " | " s
                    processes <- startPrograms pgms
                    sequence $ map (\pid -> getProcessStatus True True pid) processes
                    return True

-- Readline and execute it
readEvalLoop = do
                maybeLine <- getCurrentDirectory >>= readline . (\s->s++"$> ")
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
        readInitFile "/etc/inputrc"
        readEvalLoop
