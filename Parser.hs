module Parser where
import Text.Parsec

data Expression = Expression String String [[String]] Bool | None deriving Show

makeExpr :: [[[Char]]] -> Expression
makeExpr lst = Expression (stdInRedirect (head lst))
                          (stdOutRedirect (head (reverse lst)))
                          (map (\l -> takeWhile (\s -> s /= "<" && s /= ">" && not (elem '&' s)) l) lst)
                          (elem "&" (last lst))
    where
        stdInRedirect l = if elem "<" l then last $ dropWhile (\s -> s /= "<") l else ""
        stdOutRedirect l = if elem ">" l then last $ dropWhile (\s -> s /= ">") l else ""

word = (many1 (alphaNum <|> (oneOf "-_=./:@&"))) <|> (fmap (:[]) (oneOf "<>"))

pipe = do
        many space
        char '|'
        many space

command = do
            many space
            wds <- try (endBy word (many space)) 
            many space
            return wds

parser = sepBy1 command pipe

main = do
        let parsed = parse parser "(source)" ""
        case parsed of
            Right v -> putStrLn $ show $ makeExpr v
            otherwise -> putStrLn "Fuck"
