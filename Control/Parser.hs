{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module AbLib.Control.Parser
   ( module AbLib.Control.Parser
   , module Control.Monad
   , module Control.Applicative
) where

import Control.Monad
import Control.Applicative

import Data.List
import Data.Maybe

--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Result a}
type Result a = [(a, String)]

{- Run a custom parser -}
runParserM :: (Monad m) => Parser a -> String -> m a
runParserM p s = case filter (null . snd) $ runParser p s of
   [(x,"")] -> return x
   _        -> fail "AbLib.Control.Parser.parse: no parse"

{- Unpacks a Parser. -}
parseS :: (Parse a) => String -> Result a
parseS = runParser parser

{- Full Parsing in a safe way. -}
parseM :: (Parse a, Monad m) => String -> m a
parseM = runParserM parser

{- Equivalent to read. -}
parse :: (Parse a) => String -> a
parse = maybe parseErr id . parseM
   where parseErr = errorWithoutStackTrace "AbLib.Control.Parser.parse: no parse"

class Parse a where
   parser :: Parser a

--------------------------------------------------------------------------------

instance Functor Parser where
   fmap f p = Parser $ \ s ->
      [ (f x, r) | (x, r) <- runParser p s ]

instance Applicative Parser where
   pure x = Parser $ \ s -> [(x, s)]
   f <*> p = Parser $ \ s ->
      [ (g x, r') | (g, r) <- runParser f s, (x, r') <- runParser p r ]
      
instance Alternative Parser where
   empty = Parser $ const []
   p <|> q = Parser $ \ s -> runParser p s ++ runParser q s

{- Repeat a parsers exactly n times. -}
exactly :: Int -> Parser a -> Parser [a]
exactly n = between (n,n)

{- Repeat a parser between a and b times. -}
between :: (Int, Int) -> Parser a -> Parser [a]
between (a,b) f = do
   xs <- many f
   guard (a <= length xs && length xs <= b)
   return xs

instance Monad Parser where
   fail _ = empty
   p >>= f = Parser $ \ s -> do { (x,r) <- runParser p s; runParser (f x) r }

instance MonadPlus Parser
   {- grants access to mfilter et al. -}

instance Semigroup (Parser a) where
   (<>) = (<|>)
   
instance Monoid (Parser a) where
   mempty = empty
   
{- Parses without consuming characters -}
peek :: Parser a -> Parser a
peek f = Parser $ \s -> [ (x,s) | (x,_) <- runParser f s ]
   
{- Applies a function to the input string -}
inputMap :: (String -> String) -> Parser ()
inputMap f = Parser $ \s -> [ ((), f s) ]

{- Only returns results with minimal remaining input -}
greedy :: Parser a -> Parser a
greedy f = Parser $ \s -> let
   result = sortOn (length . snd) $ runParser f s
   maxlen = length $ snd $ head result
   in takeWhile ((maxlen==) . length . snd) result

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
onFail p q = Parser $ \s -> case runParser p s of
   [] -> runParser q s
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

maybeP :: Parser a -> Parser (Maybe a)
maybeP f = return Nothing <|> (Just <$> f)

eitherP :: Parser a -> Parser b -> Parser (Either a b)
eitherP f g = (Left <$> f) <|> (Right <$> g)


--------------------------------------------------------------------------------

instance Parse Char where
   parser = next
   
instance Parse a => Parse [a] where
   parser = many parser
   
instance Parse Int where
   parser = Parser $ readsPrec 0
