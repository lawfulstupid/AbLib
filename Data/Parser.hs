-- Provides basic parsers and combinators to build complex parsers.

module AbLib.Data.Parser {-# DEPRECATED "Use AbLib.Control.Parser instead" #-} where

import AbLib.Control.Safe
import Data.List (nub, sort, isPrefixOf)
import Data.Text

type Parser a = String -> [(a, String)]

-- Fully implement a Parser
parse :: Eq a => Parser a -> String -> a
parse p s = case nub . filter (null . snd) $ p s of 
   [x] -> fst x
   _   -> errorWithoutStackTrace "no parse"

-- Make simple substitutions to a string before parsing.
substitute :: [(String, String)] -> String -> String
substitute _ [] = []
substitute mp s = case (options mp <||> chars 1) s of
   (t,r):_ -> t ++ substitute mp r

-------- PARSER COMBINATORS --------

-- Option between two Parsers.
(<||>) :: Parser a -> Parser a -> Parser a
f <||> g = \s -> f s ++ g s
-- USE: (<>), (<|>)

-- Sequence Parsers, keep second result.
(&>) :: Parser a -> Parser b -> Parser b
f &> g = \s -> [ (x,q) | (_,r) <- f s, (x,q) <- g r ]
-- USE: (>>)

-- Convenient synonym.
(&) :: Parser a -> Parser b -> Parser b
(&) = (&>)
-- USE: (>>)

-- Sequence Parsers, keep first result.
(<&) :: Parser a -> Parser b -> Parser a
f <& g = \s -> [ (x,q) | (x,r) <- f s, (_,q) <- g r ]
-- USE: (<<)

-- Sequence Parsers, combine results
(<&>) :: Parser a -> Parser b -> Parser (a,b)
f <&> g = \s -> [ ((x,y),q) | (x,r) <- f s, (y,q) <- g r ]
-- USE: >>= \x -> fmap (x,)

-- Transform results of parsing.
(~>) :: Parser a -> (a -> b) -> Parser b
f ~> t = \s -> [ (t x, r) | (x, r) <- f s ]
-- USE: `flip fmap`

-- Transform results of parsing (in reverse).
(<~) :: (a -> b) -> Parser a -> Parser b
t <~ f = \s -> [ (t x, r) | (x, r) <- f s ]
-- USE: `fmap`

-- Repeat a Parser at least once.
many :: Parser a -> Parser a
many f = f <||> (f & many f)
-- USE: some

-- Repeat a parser any number of times.
some :: Parser a -> Parser a
some = optional . many
-- USE: many

-- Repeat a parser at most once.
-- Only use in a sequence where this result is to be discarded.
optional ::  Parser a -> Parser a
optional f = accept undefined <||> f
-- USE: a better method

-- Combines a collection of parsers disjunctively.
parallel :: Foldable t => t (Parser a) -> Parser a
parallel = foldr (<||>) reject
-- USE: mconcat

-- Accepts the first successful parser from a list.
firstRule :: [Parser a] -> Parser a
firstRule []     _ = []
firstRule (p:ps) s = case p s of
   [] -> firstRule ps s
   xs -> xs
-- USE: mconcat

-- Use a Parser without consuming input.
lookAhead :: Parser a -> Parser a
lookAhead f = \s -> [ (x,s) | (x,r) <- f ]

-------- BASIC PARSERS --------

-- Fails on all input.
reject :: Parser a
reject = const []

-- Consumes no tokens, "parsing" the supplied value.
accept :: a -> Parser a
accept x = \s -> [(x,s)]

-- Accepts a fixed number of any characters. Fails if the supplied string does not have sufficient characters.
chars :: Int -> Parser String
chars n = \ s -> let
   s' = splitAt n s
   in if n == length (fst s') then [s'] else []

-- Match a given string.
match :: String -> Parser String
match t = \ s -> let
   s' = splitAt (length t) s
   in if t == fst s' then [s'] else []

-- Uses a lookup list as a Parser.
options :: [(String, a)] -> Parser a
options = foldr1 (<||>) . map (\ (s,x) -> match s ~> const x)

-- Parse a list of items. Args: Parser for items; Delimiter string.
list :: Parser a -> String -> Parser [a]
list item delim = let
   emptyLst = accept []
   lstHead = item
   lstTail = emptyLst <||> (match delim & list item delim)
   in emptyLst <||> ((lstHead <&> lstTail) ~> uncurry (:))

-------- OFTEN-USED PARSERS --------

-- Matches any String.
anything :: Parser String
anything = \s -> [splitAt i s | i <- [0 .. length s]]

-- Matches any character.
anyChar :: Parser Char
anyChar (c:s) = [(c,s)]
anyChar _     = []

-- Matches one of the given characters.
oneOf :: [Char] -> Parser Char
oneOf cs = parallel [match [c] ~> const c | c <- cs]

-- Match a space.
space :: Parser String
space = match " "

-- Match a block of contiguous whitespace.
spaces :: Parser String
spaces = many space

