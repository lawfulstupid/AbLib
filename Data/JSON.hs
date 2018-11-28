module AbLib.Data.JSON where

import Data.Decimal
import Data.Char (toLower)
import AbLib.Data.Tuple
import AbLib.Control.Parser
import Control.Applicative ((<|>))

data JSON
   = JObj [JKVP]
   | JLst [JSON]
   | JStr String
   | JNum Decimal
   | JBoo Bool
   | JNul

data JKVP = JKVP String JSON  -- JSON Object key-value pair representation

instance Show JSON where
   show = \case
      JObj x -> "{" ++ (init . tail $ show x) ++ "}"
      JLst l -> show l
      JStr s -> show s
      JNum n -> show n
      JBoo b -> map toLower $ show b
      JNul   -> "null"
         
instance Show JKVP where
   show (JKVP k v) = show k ++ ":" ++ show v
   
instance Read JSON where
   readsPrec _ = apply parseJSON

parseJSON :: Parser JSON
parseJSON = mconcat [parseJObj, parseJLst, parseJStr, parseJNum, parseJBoo, parseJNul]

parseJObj :: Parser JSON
parseJObj = fmap JObj $ parseList ('{',',','}') $ parseJKVP

parseJKVP :: Parser JKVP
parseJKVP = do
   k <- reader
   match ':'
   v <- parseJSON
   return $ JKVP k v

parseJLst :: Parser JSON
parseJLst = fmap JLst $ parseList ('[',',',']') $ parseJSON

parseJStr :: Parser JSON
parseJStr = fmap JStr reader

parseJNum :: Parser JSON
parseJNum = fmap JNum reader

parseJBoo :: Parser JSON
parseJBoo = mconcat $ map (matchShow . JBoo) [False ..]

parseJNul :: Parser JSON
parseJNul = matchShow JNul