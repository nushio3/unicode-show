{- |
Copyright   : (c) Takayuki Muranushi, 2016
License     : MIT
Maintainer  : muranushi@gmail.com
Stability   : experimental


Provides a interactive printer for printing Unicode characters in ghci REPL.

With 'print' :

@
$ __ghci__
...
Ok, modules loaded: Text.Show.Unicode.
> __["哈斯克尔7.6.1"]__
["\\21704\\26031\\20811\\23572\\&7.6.1"]
>
@

With 'uprint' :

@
$ __ghci -interactive-print=Text.Show.Unicode.uprint Text.Show.Unicode__
...
Ok, modules loaded: Text.Show.Unicode.
> __("Хорошо!",["哈斯克尔7.6.1的力量","感じる"])__
("Хорошо!",["哈斯克尔7.6.1的力量","感じる"])
> "改\\n行"
"改\\n行"
@

Add following lines to your @~/.ghci@ config file to make 'uprint' the default interactive printer.

@
import qualified Text.Show.Unicode
:set -interactive-print=Text.Show.Unicode.uprint
@

-}

module Text.Show.Unicode (ushow, uprint, ushowWith, uprintWith) where

import Control.Applicative ((<$>), (<$), (<|>))
import GHC.Show (showLitChar)
import GHC.Read (readLitChar)
import Data.Char(isPrint)
import Text.ParserCombinators.ReadP


-- | Parse one Haskell character literal expression from a 'String' produced by 'show', and
--
--  * If the found char satisfies the predicate, replace the literal string with the character itself.
--  * Otherwise, leave the string as it is.
--  * If delimiter character '\&' is found, replace it with a null string. c.f.  <https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6 Section 2.6 of the Haskell 2010 specification>.

recoverChar :: (Char -> Bool) -> ReadP [Char]
recoverChar p = (represent <$> readS_to_P readLitChar) <|> ([] <$ string "\\&")
  where
    represent :: Char -> String
    represent c | p c = [c]
    represent c       = showLitChar c ""

-- | Parse many Haskell character literals from the input,
-- and concatenate them.
reparse :: (Char -> Bool) -> ReadP [Char]
reparse p = concat <$> many (recoverChar p)


-- | Show the input, and then replace Haskell character literals
-- with the character it represents, for any Unicode printable characters but '\\'. If something fails,
-- fallback to standard 'show'.
ushow :: Show a => a -> String
ushow = ushowWith (\c -> isPrint c && c /= '\\')

-- | A version of 'print' that uses 'ushow'.
uprint :: Show a => a -> IO ()
uprint = putStrLn . ushow


-- | Show the input, and then replace character literals
-- with the character itself, for characters that satisfy the given predicate.
ushowWith :: Show a => (Char -> Bool) -> a -> String
ushowWith p x = let showx = show x in case readP_to_S (reparse p) $ showx of
           [] -> showx
           ys -> case last ys of
             (ret,"") -> ret
             _        -> showx

-- | A version of 'print' that uses 'ushowWith'.
uprintWith :: Show a => (Char -> Bool) -> a -> IO ()
uprintWith p = putStrLn . ushowWith p
