module Token where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Symtab (Id(..))


type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

blockComment :: Parser ()
blockComment = L.skipBlockComment "\"\"\"" "\"\"\""

scn :: Parser ()
scn = L.space space1 lineComment blockComment

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment blockComment
-- sc = L.space space1 lineComment blockComment

-- -- Some things aren't really reserved words (e.g., 'yield', 'Vector2',
-- -- etc.). For example, it's possible to define a member function
-- -- called 'yield' in a script, and refer to it by 'self.yield'.
-- reservedNames =
--   ["if", "elif", "else", "for", "do", "while", "match", "switch",
--    "case", "break", "continue", "pass", "return", "class", "extends",
--    "is", "self", "tool", "signal", "func", "static", "const", "enum",
--    "var", "onready", "export", "setget", "breakpoint", "preload",
--    "yield", "assert", "remote", "master", "puppet", "remotesync",
--    "mastersync", "puppetsync", "PI", "TAU", "INF", "NAN", "true",
--    "false"]

-- reservedOps =
--   ["+", "-", "*", "/", "is", "~", "-", "%", "<<", ">>", "&", "^", "|",
--    "<", ">", "<=", ">=", "==", "!=", "in", "!", "not", "&&", "and",
--    "||", "or", "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "$", ":"]


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol s = lexeme $ string s

quotation :: Parser Char
quotation = char '\'' <|> char '"'

charLiteral :: Parser Char
charLiteral = between quotation quotation L.charLiteral

stringLiteral :: Parser String
stringLiteral = lexeme $ quotation *> manyTill L.charLiteral quotation

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

signedInteger :: Parser Integer
signedInteger = L.signed sc integer

signedFloat :: Parser Double
signedFloat = L.signed sc float

keyword :: String -> Parser String
-- keyword k = lexeme (string k <* notFollowedBy alphaNumChar)
-- Add 'try' here so it doesn't consume the string k when
-- 'notFollowedBy' fails.
keyword k = (lexeme . try) (string k <* notFollowedBy (alphaNumChar <|> char '_'))

ident :: Parser Id
ident = Id <$> lexeme
  ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
   <?> "variable")

-- withPredicate1
--   :: (a -> Bool)       -- ^ The check to perform on parsed input
--   -> String            -- ^ Message to print when the check fails
--   -> Parser a          -- ^ Parser to run
--   -> Parser a          -- ^ Resulting parser that performs the check
-- withPredicate1 f msg p = do
--   r <- lookAhead p
--   if f r
--     then p
--     else fail msg

-- withPredicate2
--   :: (a -> Bool)       -- ^ The check to perform on parsed input
--   -> String            -- ^ Message to print when the check fails
--   -> Parser a          -- ^ Parser to run
--   -> Parser a          -- ^ Resulting parser that performs the check
-- withPredicate2 f msg p = do
--   o <- getOffset
--   r <- p
--   if f r
--     then return r
--     else do
--       setOffset o
--       fail msg


-- | @postfixChain p op@ is used to remove left recursion like
-- @chainl1@, except @op@ is a postfix operator instead of infix
postfixChain :: Parser a -> Parser (a -> a) -> Parser a
postfixChain p op = do
  x <- p
  rest x
  where
    rest x = (do f <- op
                 rest $ f x) <|> return x
