import System.Console.Readline
import System.Directory
import System.Environment
import System.Posix.Process
import Data.List.Split
import Control.Exception

getPaths :: IO [String]
getPaths = fmap (map (reverse . ((:) '/') . reverse)) $ fmap (splitOn ":") (getEnv "PATH")

executeWith "" = return ()
executeWith s = executeFile (head (splitOn " " s)) True (tail (splitOn " " s)) Nothing

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
        pid <- forkProcess (executeWith s)
        getProcessStatus True True pid
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
