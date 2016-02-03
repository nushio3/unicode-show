{- |
Copyright   : (c) Takayuki Muranushi, 2016
License     : MIT
Maintainer  : muranushi@gmail.com
Stability   : experimental


provides an interactive parser for pretty-printing unicodes in ghci REPL.

c.f. https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/interactive-evaluation.html#ghci-interactive-print

c.f. http://stackoverflow.com/questions/5535512/how-to-hack-ghci-or-hugs-so-that-it-prints-unicode-chars-unescaped/13113906#13113906


-}


module Text.Show.Unicode (ushow, uprint) where



import Control.Applicative ((<$>), (<$), (<|>))
import Text.ParserCombinators.ReadP
import GHC.Read (readLitChar)

-- | Parse a Haskell character literal expression from a 'String' produced by 'show'.
--
-- Note that, special character '\&' may appear in the input,
-- and we need to skip them. Hence the return type '[Char]'. See Section 2.6 of the Haskell 2010 specification.
-- https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6

char' :: ReadP [Char]
char' = ((:[]) <$> readS_to_P readLitChar) <|> ([] <$ string "\\&")

-- | Parse many Haskell character literals from the input,
-- and concatenate them.
reparse :: ReadP [Char]
reparse = concat <$> many char'


-- | Show the input, and then replace Haskell character literals
-- with the character it represents. If anything fails,
-- fallback to standard 'show'.
ushow :: Show a => a -> String
ushow x = let showx = show x in case readP_to_S reparse $ showx of
           [] -> showx
           ys -> case last ys of
             (ret,"") -> ret
             _        -> showx

-- | A version of 'print' that uses 'ushow'.
uprint :: Show a => a -> IO ()
uprint = putStrLn . ushow
