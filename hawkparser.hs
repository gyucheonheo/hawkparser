import Features

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
