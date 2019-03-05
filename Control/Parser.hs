{-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}

module AbLib.Control.Parser where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))

import Data.List (stripPrefix, nub)
import Data.Maybe (maybeToList)
import Data.Char (isSpace)

import AbLib.Data.String (StringLike(..))

--------------------------------------------------------------------------------

newtype Parser a = Parser (ReadS a)

-- Unpacks a `Parser`.
apply :: Parser a -> ReadS a
apply (Parser p) = p

--------------------------------------------------------------------------------

instance Functor Parser where
   fmap f p = Parser $ \ s ->
      [ (f x, r) | (x, r) <- apply p s ]

instance Applicative Parser where
   pure x = Parser $ \ s -> [(x, s)]
   f <*> p = Parser $ \ s ->
      [ (g x, r') | (x, r) <- apply p s, (g, r') <- apply f r ]
      
instance Alternative Parser where
   empty = Parser $ const []
   p <|> q = Parser $ \ s -> apply p s ++ apply q s
   -- Default definitions for `many` and `some` didn't halt for Parser:
   many p = pure [] <|> some p   -- empty list OR at least one
   some p = do
      h <- p                     -- list head
      t <- many p                -- zero or more tail elements
      return (h:t)               -- stick the list together

instance Monad Parser where
   fail _ = empty
   p >>= f = Parser $ \ s -> do { (x,r) <- apply p s; apply (f x) r }

instance MonadPlus Parser        -- for posterity

instance Semigroup (Parser a) where
   (<>) = (<|>)
   
instance Monoid (Parser a) where
   mempty = empty

--------------------------------------------------------------------------------

-- Matches a given `String` exactly.
match :: StringLike s => s -> Parser String
match t = let
   t' = toString t
   in Parser $ \s -> map (t' ,) . maybeToList $ stripPrefix t' s

-- Match a given `String` and interpret it as another value.
matchAs :: String -> a -> Parser a
matchAs t x = fmap (const x) $ match t

-- Match a value by its `String` form.
matchShow :: Show a => a -> Parser a
matchShow t = matchAs (show t) t

-- Match one of the given `StringLike`s.
matchOne :: StringLike s => [s] -> Parser String
matchOne = mconcat . map match

-- Parses `Char`s that pass a test.
-- Useful in combination with `Data.Char` functions like isSpace.
matchIf :: (Char -> Bool) -> Parser Char
matchIf f = Parser $ \ (c:s) -> [ (c,s) | f c ]

-- Use a `Read` definition to parse.
reader :: Read a => Parser a
reader = Parser $ readsPrec 0

-- Match any `String`.
anything :: Parser String
anything = Parser $ \s -> map (`splitAt` s) [0 .. length s]

-- Attempts to use a `Parser`, and switches to the second only if the first fails.
onFail :: Parser a -> Parser a -> Parser a
onFail p q = Parser $ \s -> case apply p s of
   [] -> apply q s
   xs -> xs

-- Parse a list of parseable items using given delimitor.
parseList :: StringLike s => (s, s, s) -> Parser a -> Parser [a]
parseList (left, delim, right) item = do
   match left                             -- left bracket required
   x <- return [] <|> do                  -- following is optional
      h <- item                              -- first item
      t <- many (match delim >> item)        -- 0+ trailing items
      return (h : t)                         -- assemble list
   match right                            -- right bracket required
   return x

--------------------------------------------------------------------------------

-- Matches a region of contiguous whitespace
whitespace :: Parser String
whitespace = many $ matchIf isSpace













