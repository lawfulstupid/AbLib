{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module AbLib.Control.Parser
   ( module AbLib.Control.Parser
   , MonadPlus(..), Alternative(..), mfilter, liftA2, liftA3
   , (<<)
) where

import Control.Applicative (Alternative(..), liftA2, liftA3)
import Control.Monad (MonadPlus(..), mfilter)

import Data.List (stripPrefix, nub)
import Data.Maybe (maybeToList)

import AbLib.Data.String (ToString(..))
import AbLib.Control.Alias ((<<))

--------------------------------------------------------------------------------

newtype Parser a = Parser (ReadS a)

-- Unpacks a `Parser`.
apply :: Parser a -> ReadS a
apply (Parser p) = p

maybeParse :: Parser a -> String -> Maybe a
maybeParse f s = case apply f s of
   [(x,"")] -> Just x
   _        -> Nothing

fullParse :: Parser a -> String -> a
fullParse f = maybe (error "no parse") id . maybeParse f

class Parse a where
   parser :: Parser a
   parser = Parser parse
   
   parse  :: ReadS a
   parse  = apply parser
   
   {-# MINIMAL parser | parse #-}
   
instance Read a => Parse a where
   parser = reader

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
   some p = do                   -- one or more
      h <- p                     -- list head
      t <- many p                -- zero or more tail elements
      return (h:t)               -- stick the list together

instance Monad Parser where
   fail _ = empty
   p >>= f = Parser $ \ s -> do { (x,r) <- apply p s; apply (f x) r }

instance MonadPlus Parser
   -- grants access to mfilter et al.

instance Semigroup (Parser a) where
   (<>) = (<|>)
   
instance Monoid (Parser a) where
   mempty = empty
   
optional :: Parser a -> Parser (Maybe a)
optional f = matchAs "" Nothing <|> fmap Just f
   
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- Matches a value exactly using its String representation
match :: ToString a => a -> Parser a
match t = let
   t' = toString t
   in Parser $ \s -> map (t ,) . maybeToList $ stripPrefix t' s

-- Match a given `String` and interpret it as another value.
matchAs :: ToString k => k -> a -> Parser a
matchAs t x = fmap (const x) $ match t

-- Match one of the given values
matchOne :: ToString a => [a] -> Parser a
matchOne = mconcat . map match

-- Parses values pass a test.
-- Useful in combination with `Data.Char` functions like isSpace.
matchIf :: (Char -> Bool) -> Parser Char
matchIf f = Parser $ \case
   []    -> []
   (c:s) -> [ (c,s) | f c ]

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
parseList :: ToString s => (s, s, s) -> Parser a -> Parser [a]
parseList (left, delim, right) item = do
   match left                             -- left bracket required
   x <- return [] <|> do                  -- following is optional
      h <- item                              -- first item
      t <- many (match delim >> item)        -- 0+ trailing items
      return (h : t)                         -- assemble list
   match right                            -- right bracket required
   return x

--------------------------------------------------------------------------------
