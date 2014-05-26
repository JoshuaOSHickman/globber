module Text.Globber where
import Data.List (tails)

data PatternPart = Literal Char
                 | SingleWild
                 | StarWild
                 | SetMatch [Char]

parse :: String -> [PatternPart]
parse "" = []
parse ('?':s) = SingleWild : parse s
parse ('*':s) = StarWild : parse s
parse ('[':s) = let (matchOptions, rest) = parseSetMatch s
                in SetMatch matchOptions : parse rest
parse ('\\':c:s) = Literal c : parse s
parse (x:s) = Literal x : parse s

parseSetMatch :: String -> ([Char], String)
parseSetMatch [] = ([], "")
parseSetMatch (']':rest) = ([], rest)
parseSetMatch ('\\':literal:rest) = (literal : others, continue)
    where (others, continue) = parseSetMatch rest 
parseSetMatch (a:'-':b:rest) | b /= ']' = (enumFromTo a b ++ others, continue)
                             where (others, continue) = parseSetMatch rest
parseSetMatch (a:rest) = (a : others, continue) 
    where (others, continue) = parseSetMatch rest

matches :: [PatternPart] -> String -> Bool
matches (Literal c1 : rest) (c2:cs) = c1 == c2 && matches rest cs
matches (SingleWild : rest) (c:cs) = matches rest cs
matches (SetMatch matchChars : rest) (c:cs) = c `elem` matchChars && matches rest cs
matches (StarWild : rest) cs = any (matches rest) (tails cs)
matches [] [] = True
matches _ _ = False

globMatch :: String -> String -> Bool
globMatch = matches . parse
