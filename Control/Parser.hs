{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module AbLib.Control.Parser
   ( module AbLib.Control.Parser
   , MonadPlus(..), Alternative(..), mfilter, liftA2, liftA3, guard
   , (<<)
) where

import Control.Applicative (Alternative(..), liftA2, liftA3)
import Control.Monad (MonadPlus(..), mfilter, guard)

import Data.List (stripPrefix, nub)
import Data.Maybe (maybeToList)

-- import AbLib.Data.String (ToString(..))
import AbLib.Control.Alias ((<<))

--------------------------------------------------------------------------------

newtype Parser a = Parser (String -> [(a, String)])

{- Unpacks a Parser. -}
apply :: Parser a -> ReadS a
apply (Parser p) = p

{- Runs a Parser, return the parsed value. -}
fullParse :: Parser a -> String -> a
fullParse f = maybe parseErr id . maybeParse f
   where parseErr = errorWithoutStackTrace "AbLib.Control.Parser.fullParse: no parse"

{- Full Parsing in a safe way. -}
maybeParse :: Parser a -> String -> Maybe a
maybeParse f s = case apply f s of
   [(x,"")] -> Just x
   _        -> Nothing

class Parse a where
   parser :: Parser a
   parser = Parser parse
   
   parse :: ReadS a
   parse = apply parser
   
   {-# MINIMAL parser | parse #-}

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
   {- Default definitions for `many` and `some` didn't halt. -}
   many p = pure [] <|> some p   -- empty list OR at least one
   some p = do                   -- one or more
      h <- p                     -- list head
      t <- many p                -- zero or more tail elements
      return (h:t)               -- stick the list together

{- Repeat a parsers exactly n times -}
exactly :: Int -> Parser a -> Parser [a]
exactly n f = sequence $ replicate n f

instance Monad Parser where
   fail _ = empty
   p >>= f = Parser $ \ s -> do { (x,r) <- apply p s; apply (f x) r }

instance MonadPlus Parser
   {- grants access to mfilter et al. -}

instance Semigroup (Parser a) where
   (<>) = (<|>)
   
instance Monoid (Parser a) where
   mempty = empty
   
{- Makes a Parser optional, realised through Maybe -}
optional :: Parser a -> Parser (Maybe a)
optional f = matchAs "" Nothing <|> fmap Just f

{- Parses without consuming characters -}
peek :: Parser a -> Parser a
peek f = Parser $ \s -> [ (x,s) | (x,_) <- apply f s ]
   
{- Applies a function to the input string -}
inputMap :: (String -> String) -> Parser ()
inputMap f = Parser $ \s -> [ ((), f s) ]

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

{- Returns the next character from input. -}
next :: Parser Char
next = Parser $ \case
   []    -> []
   (c:s) -> [(c,s)]

{- Matches a value exactly using its String representation. -}
match :: (Parse a, Eq a) => a -> Parser a
match t = parser >>= guard . (t ==) >> return t

{- Match a given `String` and interpret it as another value. -}
matchAs :: (Parse k, Eq k) => k -> a -> Parser a
matchAs t x = fmap (const x) $ match t

{- Match one of the given values -}
matchOne :: (Eq a, Parse a) => [a] -> Parser a
matchOne = mconcat . map match

{- Parses values pass a test. -}
{- Useful in combination with `Data.Char` functions like isSpace. -}
matchIf :: (Char -> Bool) -> Parser Char
matchIf f = do
   c <- next
   guard (f c)
   return c

{- Use a `Read` definition to parse. -}
{- Largely redundant since instance Read a => Parse a was introduced. -}
{- Felt cute, might deprecate later idk -}
reader :: Read a => Parser a
reader = Parser $ readsPrec 0

{- Match any String. -}
anything :: Parser String
anything = many next
-- anything = Parser $ \s -> map (`splitAt` s) [0 .. length s]

{- Attempts to use a `Parser`, and switches to the second only if the first fails. -}
onFail :: Parser a -> Parser a -> Parser a
onFail p q = Parser $ \s -> case apply p s of
   [] -> apply q s
   xs -> xs

{- Parse a list of parseable items using given delimitor. -}
parseList :: (Eq s, Parse s) => (s, s, s) -> Parser a -> Parser [a]
parseList (left, delim, right) item = do
   match left                             -- left bracket required
   x <- return [] <|> do                  -- following is optional
      h <- item                              -- first item
      t <- many (match delim >> item)        -- 0+ trailing items
      return (h : t)                         -- assemble list
   match right                            -- right bracket required
   return x
   


--------------------------------------------------------------------------------

instance Parse Char where
   parser = next
   
instance Parse String where
   parser = anything
   
instance Parse Int where
   parser = Parser $ readsPrec 0