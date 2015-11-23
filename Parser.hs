module Parser where
import Text.Parsec

data Expression = Expression String String [[String]] Bool | Exit | CD String deriving Show

makeExpr :: [[[Char]]] -> Expression
makeExpr lst
    | head (head (lst)) == "cd" = if (length (head lst)) > 1 then CD (head (drop 1 (head lst))) else CD ""
    | head (head (lst)) == "exit" = Exit
    | otherwise = Expression (stdInRedirect (head lst))
                          (stdOutRedirect (head (reverse lst)))
                          (map (\l -> takeWhile (\s -> s /= "<" && s /= ">" && not (elem '&' s)) l) lst)
                          (elem "&" (last lst))
    where
        stdInRedirect l = if elem "<" l then last $ dropWhile (\s -> s /= "<") l else ""
        stdOutRedirect l = if elem ">" l then last $ dropWhile (\s -> s /= ">") l else ""
word = (many1 (alphaNum <|> (oneOf "-_=./:@&\""))) <|> (fmap (:[]) (oneOf "<>"))

str = do
        char '\''
        wds <- (endBy word (many space))
        char '\''
        return (concat (map (\s -> s++" ") wds))

pipe = do
        many space
        char '|'
        many space

command = do
            many space
            wds <- try (endBy (word <|> str) (many space)) 
            many space
            return wds

parser = sepBy1 command pipe

main = do
        let parsed = parse parser "(source)" "awk 'abc def'"
        case parsed of
            Right v -> putStrLn $ show $ v
            otherwise -> putStrLn "Fuck"
