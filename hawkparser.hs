import Data.Char
import Data.List

data Ptree = ID String | VAR String | FCN String [Ptree] deriving (Show, Read)

data State s a = State {
  run :: s -> Maybe (a, s)
}

instance Functor (State s) where
  fmap f st = State $ \s -> case run st s of
                              Nothing -> Nothing
                              Just (x, s') -> Just (f x, s')

instance Applicative (State s) where
  pure x = State $ \s -> Just (x, s)
  stf <*> stx = State $ \s -> case run stf s of
                                Nothing -> Nothing
                                Just (f, s') -> run (fmap f stx) s'

instance Monad (State s) where
  st >>= f = State $ \s -> case run st s of
                             Nothing -> Nothing
                             Just (x, s') -> run (f x) s'

type Parser a = State String a

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

  many :: f a -> f [a] -- zero or more
  some :: f a -> f [a] -- one or more

  many x = some x <|> pure []
  some x = pure (:) <*> x <*> many x

instance Alternative (State s) where
  empty = State $ \s -> Nothing

  p <|> q = State $ \s -> case run p s of
                            Nothing -> run q s
                            r -> r                
expression :: Parser Ptree
expression = do l <- (variable <|> functionCall <|> identifier)
                return l

identifier :: Parser Ptree
identifier = do l <- lower_letter
                ls <- many (alphanum <|> underscore)
                return $ ID (l:ls)

variable :: Parser Ptree
variable =  do l <- upper_letter
               ls <- many (alphanum <|> underscore)
               return $ VAR (l:ls)

functionCall :: Parser Ptree
functionCall = do n <- identifier
                  symbol "("
                  args <- arguments
                  symbol ")"
                  return $ FCN (decompose n) args

arguments :: Parser [Ptree]
arguments = do l <- expression
               ls <- argTail
               return (l:ls)
            <|> do pure []
                   return []


argTail :: Parser [Ptree]
argTail = do symbol ","
             l <- expression
             ls <- argTail
             return (l:ls)
          <|> do pure []
                 return []

item :: Parser Char
item = State $ \str -> case str of "" -> Nothing
                                   (c:cs) -> Just (c, cs)

sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           if p c then return c else empty

decompose a = case a of
                ID x -> x
                VAR x -> x
                
isUnderscore :: Char -> Bool
isUnderscore x = if x == '_' then True else False

digit :: Parser Char
digit = sat isDigit

letter :: Parser Char
letter = sat isAlpha

lower_letter :: Parser Char
lower_letter = sat isLower

upper_letter :: Parser Char
upper_letter = sat isUpper

alphanum :: Parser Char
alphanum = sat isAlphaNum

underscore :: Parser Char
underscore = sat isUnderscore

nat :: Parser Int
nat = do cs <- some digit
         return (read cs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

space :: Parser ()
space = do many (sat isSpace)
           return ()

char :: Char -> Parser Char
char c = sat (==c)

string :: String -> Parser String
string "" = return ""
string (x:xs) = do char x
                   string xs
                   return (x:xs)

token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x

symbol :: String -> Parser String
symbol s = token (string s)

integer :: Parser Int
integer = token int
