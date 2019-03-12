module Parser where

import Control.Monad (void)

import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Maybe (fromMaybe)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Ast
import Symtab (Id(..))
import Token
import Util (debugPrint)


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

commaSep1 :: Parser a -> Parser [a]
commaSep1 p = p `sepBy1` symbol ","

bool :: Parser Bool
bool = choice
  [ keyword "true" >> return True
  , keyword "false" >> return False ]

dictEntry :: Parser (Expr SourcePos, Expr SourcePos)
dictEntry = do
  e1 <- expr
  symbol ":"
  e2 <- expr
  return (e1, e2)

literal :: Parser (Literal SourcePos)
literal = choice
  [ LBool   <$> bool
  , LInt    <$> integer
  , LFloat  <$> float
  , LString <$> stringLiteral
  , LArray  <$> brackets (commaSep expr)
  , LDict   <$> brackets (commaSep dictEntry) ]

call :: SourcePos -> Parser (Expr SourcePos)
call s = do
  f <- EIdent s <$> ident
  args <- parens $ commaSep expr
  return $ ECall s f args

term :: Parser (Expr SourcePos)
term = do
  s <- getSourcePos
  choice
    [ try $ call s
    , ELiteral s <$> literal
    , EIdent s <$> ident
    , parens expr ]

expr :: Parser (Expr SourcePos)
expr = makeExprParser term operatorTable

binary :: String ->
          (SourcePos -> Expr SourcePos ->
           Expr SourcePos -> Expr SourcePos) ->
          Operator Parser (Expr SourcePos)
binary name f = InfixL $ do
  symbol name
  s <- getSourcePos
  return $ f s

binaryNoAssoc :: String ->
                 (SourcePos -> Expr SourcePos ->
                  Expr SourcePos -> Expr SourcePos) ->
                 Operator Parser (Expr SourcePos)
binaryNoAssoc name f = InfixN $ do
  symbol name
  s <- getSourcePos
  return $ f s

prefix, postfix :: String ->
                   (SourcePos -> Expr SourcePos -> Expr SourcePos) ->
                   Operator Parser (Expr SourcePos)
prefix  name f = Prefix $ do
  symbol name
  s <- getSourcePos
  return $ f s
postfix name f = Postfix $ do
  symbol name
  s <- getSourcePos
  return $ f s

op n ms =
  (lexeme . try) (string n <* notFollowedBy (choice (char <$> ms)))

indexOp :: Operator Parser (Expr SourcePos)
indexOp = Postfix $ do
  s <- getSourcePos
  index <- brackets expr
  return $ flip (EBinop s BIndex) index

ifelseOp :: Operator Parser (Expr SourcePos)
ifelseOp = Postfix $ do
  s <- getSourcePos
  keyword "if"
  e2 <- expr
  keyword "else"
  e3 <- expr
  return $ \e1 -> EIfElse s e1 e2 e3

-- Helper for single-character operators that overlap with others. For
-- example, we must use this for the regular addition operator '+' or
-- else '+=' won't work.
op' :: Binop -> String -> [Char] -> Operator Parser (Expr SourcePos)
op' b s cs = InfixL $ getSourcePos >>= \pos -> EBinop pos b <$ op s cs

operatorTable :: [[Operator Parser (Expr SourcePos)]]
operatorTable =  [
  [ prefix "$" $ flip EUnop UGetNode ],
  [ indexOp ],
  [ binary "." $ flip EBinop BAttribute ],
  [ binaryNoAssoc "is" $ flip EBinop BIs ],
  [ prefix "~" $ flip EUnop UBitwiseNot ],
  [ prefix "-" $ flip EUnop UNeg ],
  [ op' BMult "*" ['='], op' BDiv  "/" ['='], op' BMod  "%" ['='] ],
  [ op' BPlus  "+" ['='], op' BMinus "-" ['='] ],
  [ binary "<<" $ flip EBinop BShiftL,
    binary ">>" $ flip EBinop BShiftR],
  [ op' BBitwiseAnd "&" ['=', '&'] ],
  [ binary "^" $ flip EBinop BBitwiseXor ],
  [ op' BBitwiseOr "|" ['=', '|'] ],
  [ InfixN $ getSourcePos >>= \s -> EBinop s BLt <$ op "<" ['=']
  , InfixN $ getSourcePos >>= \s -> EBinop s BGt <$ op ">" ['=']
  , binaryNoAssoc "<=" $ flip EBinop BLe
  , binaryNoAssoc ">=" $ flip EBinop BGe
  , binaryNoAssoc "==" $ flip EBinop BEq
  , binaryNoAssoc "!=" $ flip EBinop BNeq ],
  [ binaryNoAssoc "in" $ flip EBinop BIn ],
  [ prefix "!"   $ flip EUnop UNot , prefix "not" $ flip EUnop UNot ],
  [ binary "&&"  $ flip EBinop BAnd , binary "and" $ flip EBinop BAnd ],
  [ binary "||" $ flip EBinop BOr , binary "or" $ flip EBinop BOr ],
  [ ifelseOp ],
  [ binaryNoAssoc "="  $ flip EBinop BAssign
  , binaryNoAssoc "+=" $ flip EBinop BPlusAssign
  , binaryNoAssoc "-=" $ flip EBinop BMinusAssign
  , binaryNoAssoc "*=" $ flip EBinop BMultAssign
  , binaryNoAssoc "/=" $ flip EBinop BDivAssign
  , binaryNoAssoc "%=" $ flip EBinop BModAssign
  , binaryNoAssoc "&=" $ flip EBinop BAndAssign
  , binaryNoAssoc "|=" $ flip EBinop BDivAssign ]
  ]


sreturn :: SourcePos -> Parser (Stmt SourcePos)
sreturn pos = keyword "return" >> expr >>= return . SReturn pos

spass :: SourcePos -> Parser (Stmt SourcePos)
spass pos = keyword "pass" >> return (SPass pos)

svar :: SourcePos -> Parser (Stmt SourcePos)
svar pos = do
  keyword "var"
  x <- ident
  e <- optional $ symbol "=" >> expr
  return $ SVar pos x e

sif :: SourcePos -> Parser (Stmt SourcePos)
sif pos = do
  (e, if') <- L.indentBlock scn p
  elifs <- many selif
  else' <- optional selse
  return $ SIf pos e if' elifs else'
  where
    p = do
      keyword "if"
      e <- expr
      symbol ":"
      return $ L.IndentSome Nothing
        (\stmts -> return (e, stmts)) stmt

selif :: Parser (Expr SourcePos, [Stmt SourcePos])
selif = L.indentBlock scn p
  where
    p = do
      keyword "elif"
      e <- expr
      symbol ":"
      return $ L.IndentSome Nothing
        (\stmts -> return (e, stmts)) stmt


selse :: Parser [Stmt SourcePos]
selse = L.indentBlock scn p
  where
    p = do
      keyword "else"
      symbol ":"
      return $ L.IndentSome Nothing return stmt

swhile :: SourcePos -> Parser (Stmt SourcePos)
swhile pos = do
  (e, s) <- L.indentBlock scn p
  return $ SWhile pos e s
  where
    p = do
      keyword "while"
      e <- expr
      symbol ":"
      return $ L.IndentSome Nothing
        (\stmts -> return (e, stmts)) stmt

sfor :: SourcePos -> Parser (Stmt SourcePos)
sfor pos = do
  (x, e, s) <- L.indentBlock scn p
  return $ SFor pos x e s
  where
    p = do
      keyword "for"
      x <- ident
      keyword "in"
      e <- expr
      symbol ":"
      return $ L.IndentSome Nothing
        (\stmts -> return (x, e, stmts)) stmt

arrayPattern :: Parser (Pattern SourcePos)
arrayPattern = PArray <$> brackets (commaSep simple_pattern)

-- We allow '..' to appear anywhere in the dict pattern and check
-- later that it's only in the last position (or doesn't appear at
-- all).
dictEntryPattern :: Parser (DictEntryPattern SourcePos)
dictEntryPattern =
  (symbol ".." >> return DEPOpen) <|> (uncurry DEPKV <$> p)
  where
    p = do
      k <- PLiteral <$> literal
      symbol ":"
      v <- simple_pattern
      return (k, v)

-- Open-ended dictionary: A dictionary can be bigger than the pattern
-- by making the last subpattern ..
dictPattern :: Parser (Pattern SourcePos)
dictPattern = PDict <$> braces (commaSep dictEntryPattern)

simple_pattern :: Parser (Pattern SourcePos)
simple_pattern = choice
  [ PLiteral <$> literal
  , PIdent <$> ident
  , PVar <$> (keyword "var" >> ident)
  , symbol "_" >> return PWildcard
  , arrayPattern
  , dictPattern ]

pat :: Parser (Pattern SourcePos)
pat = do
  pats <- commaSep1 simple_pattern
  case pats of
    [x] -> return x
    _ -> return $ PMulti pats

scase :: Parser (Pattern SourcePos, [Stmt SourcePos])
scase = L.indentBlock scn prs
  where
    prs = do
      p <- pat
      symbol ":"
      return $ L.IndentSome Nothing
        (\stmts -> return (p, stmts)) stmt


smatch :: SourcePos -> Parser (Stmt SourcePos)
smatch pos = do
  (e, cases) <- L.indentBlock scn p
  return $ SMatch pos e cases
  where
    p = do
      keyword "match"
      e <- expr
      symbol ":"
      return $ L.IndentSome Nothing
        (\cases -> return (e, cases)) scase


sbreak :: SourcePos -> Parser (Stmt SourcePos)
sbreak pos = keyword "break" >> return (SBreak pos)

scontinue :: SourcePos -> Parser (Stmt SourcePos)
scontinue pos = keyword "continue" >> return (SBreak pos)

sassert :: SourcePos -> Parser (Stmt SourcePos)
sassert pos = keyword "assert" >> expr >>= return . SAssert pos

sexpr :: SourcePos -> Parser (Stmt SourcePos)
sexpr pos = SExpr pos <$> expr

stmt :: Parser (Stmt SourcePos)
stmt = do
  pos <- getSourcePos
  -- debugPrint (show pos) $
  choice
    [ spass pos
    , svar pos
    , sif pos
    , swhile pos
    , sfor pos
    , smatch pos
    , sbreak pos
    , scontinue pos
    , sassert pos
    , sreturn pos
    , sexpr pos ]

-- No syntax for array or dictionary types for now.
ty :: Parser Type
ty = choice
  [ keyword "null"        >> return TNull
  , keyword "bool"        >> return TBool
  , keyword "int"         >> return TInt
  , keyword "float"       >> return TFloat
  , keyword "String"      >> return TString
  , keyword "Vector2"     >> return TVector2
  , keyword "Rect2"       >> return TRect2
  , keyword "Vector3"     >> return TVector3
  , keyword "Transform2D" >> return TTransform2D
  , keyword "Plane"       >> return TPlane
  , keyword "Quat"        >> return TQuat
  , keyword "AABB"        >> return TAABB
  , keyword "Basis"       >> return TBasis
  , keyword "Transform"   >> return TTransform
  , keyword "Color"       >> return TColor
  , keyword "NodePath"    >> return TNodePath
  , keyword "RID"         >> return TRID
  , keyword "Object"      >> return TObject
  , ident >>= return . TName ]

enumEntry :: Parser (Id, Maybe (Expr SourcePos))
enumEntry = do
  x <- ident
  e <- optional $ symbol "=" >> expr
  return (x, e)

enum :: SourcePos -> Parser (Command SourcePos)
enum pos = L.indentBlock scn $ do
  keyword "enum"
  entries <- braces $ commaSep enumEntry
  return $ L.IndentNone $ CEnum pos entries

setget :: Parser (Id, Id)
setget = do
  x <- ident
  symbol ","
  y <- ident
  return (x, y)

cvar :: SourcePos -> Parser (Command SourcePos)
cvar pos = L.indentBlock scn $ do
  (export, export_list) <-
    (do
        keyword "export"
        l <- optional $ parens $ do
          t <- ty
          es <- optional $ do
            symbol ","
            commaSep1 expr
          case es of
            Nothing  -> return (t, [])
            Just es' -> return (t, es')
        return (True, l))
    <|> return (False, Nothing)
  onready <- (keyword "onready" >> return True) <|> return False
  keyword "var"
  x <- ident
  e <- optional $ symbol "=" >> expr
  sg <- optional setget
  return $ L.IndentNone $ CVar pos export export_list onready x e sg

cconst :: SourcePos -> Parser (Command SourcePos)
cconst pos = L.indentBlock scn $ do
  keyword "const"
  x <- ident
  symbol "="
  e <- expr
  return $ L.IndentNone $ CConst pos x e

func :: SourcePos -> Parser (Command SourcePos)
func pos = L.indentBlock scn $ do
  static <- (keyword "static" >> return True) <|> return False
  keyword "func"
  f <- ident
  let f_ty = TNull
  args <- parens $ commaSep ident
  let typed_args = flip (,) TNull <$> args
  symbol ":"
  return $ L.IndentSome Nothing
    (return . (CFunc pos static f f_ty typed_args)) stmt

signal :: SourcePos -> Parser (Command SourcePos)
signal pos = do
  keyword "signal"
  nm <- ident
  args <- optional $ parens $ commaSep ident
  return $ CSignal pos nm $ fromMaybe [] args

extends :: SourcePos -> Parser (Command SourcePos)
extends pos = L.indentBlock scn $ do
  c <- keyword "extends" >> expr >>= return . CExtends pos
  return $ L.IndentNone c

command :: Parser (Command SourcePos)
command = do
  pos <- getSourcePos
  choice
    [ enum pos
    , cvar pos
    , cconst pos
    , func pos
    , extends pos
    , nestedCls pos
    , signal pos ]

nestedCls :: SourcePos -> Parser (Command SourcePos)
nestedCls pos = L.indentBlock scn $ do
  keyword "class"
  nm <- ident
  symbol ":"
  return $ L.IndentSome Nothing
    (\coms -> return $ CClass pos $
              Class { class_name = nm
                    , class_commands = coms }) command

cls :: Id -> Parser (Class SourcePos)
cls nm = L.nonIndented scn (L.indentBlock scn p)
  where      
    p = do
      coms <- some command
      return $ L.IndentNone (Class { class_name = nm
                                   , class_commands = coms })

parse :: String -> String ->
         Either (ParseErrorBundle String Void) (Class SourcePos)
parse filename src =
  runParser (cls $ Id filename) filename src
