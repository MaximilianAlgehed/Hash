import System.Console.Readline
import System.Directory
import System.Environment
import System.Posix.Process
import System.Posix.IO
import Data.List.Split
import Control.Exception

getPaths :: IO [String]
getPaths = fmap (map (reverse . ((:) '/') . reverse)) $ fmap (splitOn ":") (getEnv "PATH")

executeWith "" _ _ = return ()
executeWith s in_fd out_fd =
    do
        dupTo in_fd stdInput
        dupTo out_fd stdOutput
        executeFile (head (splitOn " " s)) True (tail (splitOn " " s)) Nothing

makeProcesses [x] in_fd out_fd =
    do 
        pid <- forkProcess (executeWith x in_fd out_fd)
        return [pid]
makeProcesses (x:xs) in_fd out_fd = 
    do
        (r, w) <- createPipe
        pid <- forkProcess $ executeWith x in_fd w
        closeFd w
        lst <- makeProcesses xs r out_fd
        closeFd r
        return (pid:lst)

fixPgms pgms = pgms
getInput pgms = stdInput
getOutput pgms = stdOutput

startPrograms pgms = makeProcesses (fixPgms pgms) (getInput pgms) (getOutput pgms)

handleInput :: String -> IO Bool
handleInput "exit" = return False
handleInput ('c':'d':xs)
    | xs == "" = (getEnv "HOME") >>= setCurrentDirectory >>= (\_ -> return True)
    | (head xs) == ' ' = do 
                            result <- try $ setCurrentDirectory (tail xs) :: IO (Either SomeException ())
                            case result of
                                Left ex -> putStrLn "Action not possible" >>= (\_ -> return True)
                                Right () -> return True
handleInput s =
    do
        let pgms = splitOn " | " s
        processes <- startPrograms pgms
        sequence $ map (\pid -> getProcessStatus True True pid) processes
        return True

readEvalLoop = do
                maybeLine <- getCurrentDirectory >>= readline . (\s->s++"$> ")
                case maybeLine of
                    Nothing -> putStrLn "" >>= return
                    Just line -> do
                                 addHistory line
                                 exit <- handleInput line 
                                 if not exit then return ()
                                    else readEvalLoop

main :: IO ()
main = do
        readInitFile "/etc/inputrc"
        readEvalLoop
