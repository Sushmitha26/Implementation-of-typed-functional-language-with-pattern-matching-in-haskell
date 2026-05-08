module Parser where

import Syntax
import Text.ParserCombinators.Parsec

ws :: Parser ()
ws = do
  many (oneOf " \n\t")
  return ()

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  ws
  return x

symbol :: String -> Parser String
symbol s = lexeme (string s)

parens :: Parser a -> Parser a
parens p = do
  symbol "("
  x <- p
  symbol ")"
  return x

brackets :: Parser a -> Parser a
brackets p = do
  symbol "["
  x <- p
  symbol "]"
  return x

identifier :: Parser String
identifier = lexeme . try $ do
  first <- letter
  rest <- many (alphaNum <|> char '_')
  let name = first : rest
  if name `elem` reservedWords
    then unexpected ("reserved word " ++ show name)
    else return name

reservedWords :: [String]
reservedWords =
  ["if", "then", "else", "let", "letrec", "in", "true", "false",
   "case", "of", "Nil", "Cons", "Int", "Bool", "List"]

parseInt :: Parser Expr
parseInt = do
  digits <- lexeme (many1 digit)
  return (EInt (read digits))

parseBool :: Parser Expr
parseBool =
      try (symbol "true" >> return (EBool True))
  <|> try (symbol "false" >> return (EBool False))

parseVar :: Parser Expr
parseVar = do
  name <- identifier
  return (EVar name)

parseTypeAtom :: Parser Type
parseTypeAtom =
      (symbol "Int" >> return TInt)
  <|> (symbol "Bool" >> return TBool)
  <|> parseListType
  <|> parens parseType

parseListType :: Parser Type
parseListType = do
  symbol "List"
  t <- parseTypeAtom
  return (TList t)

parseType :: Parser Type
parseType = do
  t1 <- parseTypeAtom
  parseArrowType t1
  where
    parseArrowType t1 =
          (do symbol "->"
              t2 <- parseType
              return (TFun t1 t2))
      <|> return t1

parseNilExpr :: Parser Expr
parseNilExpr = do
  symbol "Nil"
  t <- brackets parseType
  return (ENil t)

parseConsExpr :: Parser Expr
parseConsExpr = do
  symbol "Cons"
  e1 <- parseAtom
  e2 <- parseAtom
  return (ECons e1 e2)

parseAtom :: Parser Expr
parseAtom =
      parseInt
  <|> parseBool
  <|> parseNilExpr
  <|> try parseConsExpr
  <|> parens parseExpr
  <|> parseVar

parseApp :: Parser Expr
parseApp = do
  exprs <- many1 parseAtom
  return (foldl1 EApp exprs)

parseMul :: Parser Expr
parseMul = chainl1 parseApp mulOp

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = do
  symbol "*"
  return EMul

parseAdd :: Parser Expr
parseAdd = chainl1 parseMul addOp

addOp :: Parser (Expr -> Expr -> Expr)
addOp =
      (symbol "+" >> return EAdd)
  <|> (symbol "-" >> return ESub)

parseCmp :: Parser Expr
parseCmp = do
  e1 <- parseAdd
  parseRest e1
  where
    parseRest e1 =
          (do symbol "=="
              e2 <- parseAdd
              return (EEq e1 e2))
      <|> (do symbol "<"
              e2 <- parseAdd
              return (ELt e1 e2))
      <|> return e1

parseLam :: Parser Expr
parseLam = do
  symbol "\\"
  x <- identifier
  symbol ":"
  t <- parseTypeAtom
  symbol "->"
  body <- parseExpr
  return (ELam x t body)

parseIf :: Parser Expr
parseIf = do
  symbol "if"
  cond <- parseExpr
  symbol "then"
  eThen <- parseExpr
  symbol "else"
  eElse <- parseExpr
  return (EIf cond eThen eElse)

parseLet :: Parser Expr
parseLet = do
  symbol "let"
  x <- identifier
  symbol "="
  e1 <- parseExpr
  symbol "in"
  e2 <- parseExpr
  return (ELet x e1 e2)

parsePattern :: Parser Pattern
parsePattern =
      (symbol "Nil" >> return PNil)
  <|> parseConsPattern

parseConsPattern :: Parser Pattern
parseConsPattern = do
  symbol "Cons"
  x <- identifier
  xs <- identifier
  return (PCons x xs)

parseBranch :: Parser (Pattern, Expr)
parseBranch = do
  pat <- parsePattern
  symbol "->"
  expr <- parseExpr
  return (pat, expr)

parseCase :: Parser Expr
parseCase = do
  symbol "case"
  scrutinee <- parseExpr
  symbol "of"
  b1 <- parseBranch
  symbol "|"
  b2 <- parseBranch
  return (ECase scrutinee [b1, b2])


parseLetRec :: Parser Expr
parseLetRec = do
  symbol "letrec"
  f <- identifier
  symbol ":"
  declaredType <- parseType
  symbol "="
  e1 <- parseExpr
  symbol "in"
  e2 <- parseExpr
  return (ELetRec f declaredType e1 e2)

parseExpr :: Parser Expr
parseExpr =
      parseIf
  <|> parseLetRec
  <|> parseLet
  <|> parseLam
  <|> parseCase
  <|> parseCmp

contents :: Parser a -> Parser a
contents p = do
  ws
  result <- p
  eof
  return result

parseString :: String -> Either ParseError Expr
parseString input = parse (contents parseExpr) "<input>" input