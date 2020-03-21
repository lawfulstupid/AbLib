
{-
   Contains some common basic parsers
   Intended for qualified import
-}
module AbLib.Control.ParserUtils where

import AbLib.Control.Parser
import Data.Char (isSpace)

{- Matches a nonempty region of contiguous whitespace -}
{- Includes newlines -}
whitespace :: Parser String
whitespace = some $ matchIf isSpace

{- Matches a nonempty region of contiguous inline whitespace -}
{- Does not include newlines -}
ws :: Parser String
ws = some $ matchOne " \t"

{- Matches one newline character -}
newline :: Parser Char
newline = matchOne "\n\r\f"

-- Matches a letter
letter :: Parser Char
letter = letter_upper <|> letter_lower

-- Matches an uppercase letter
letter_upper :: Parser Char
letter_upper = matchOne ['A'..'Z']

-- Matches an lowercase letter
letter_lower :: Parser Char
letter_lower = matchOne ['a'..'z']

-- Matches a digit
digit :: Parser Char
digit = matchOne ['0'..'9']

-- Matches an alphanumeric character
alphanum :: Parser Char
alphanum = letter <|> digit

-- Matches a normal identifier format
ident :: Parser String
ident = liftA2 (:) letter $ many (alphanum <|> match '_')

-- Matches end of input
eof :: Parser ()
eof = Parser $ \s -> [((),s) | null s]