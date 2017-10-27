
{-Consider the following programming language kernel.

data Value = B Bool | I Int
  deriving (Eq, Show)

data Expr =
  Lit Value |
  Var String |
  Add Expr Expr |
  Sub Expr Expr |
  Mul Expr Expr |
  Div Expr Expr |
  Lambda String Expr |
  IfElse Expr Expr Expr
  deriving (Eq, Show)

Express this type as fixed point of a functor just like you did in E3 and provide pattern synonyms to retain compatability with the original (you will need the PatternSynonyms extension for this).

If you want, you can use the provided skeleton program to get started.-}

-- #!/usr/bin/env stack
-- stack --resolver lts-9.6 script --package megaparsec --package text

-- NOTE. To run this file (Expr.hs), do
-- chmod +x Expr.hs
-- echo "\x -> if(y) then x+15 else x"|./ExprL.hs

{-#LANGUAGE PatternSynonyms#-}
{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE DeriveFunctor#-}
{-#LANGUAGE DeriveTraversable#-}

import Text.Megaparsec.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Void
import Data.Fix


--Toimii windowssilla n‰in:
--komento.txt < "\x -> if(y) then x+15 else x"
--poista teksti tiedostosta sinne j‰‰nee ""
--stack exec stakki < komento.txt
--testattu: 5-5+2, \x -> if(y) then x+15 else x
main = do
        input <- TIO.getContents
        case runParser (expr<*optional eol<*eof) "stdin" input of
            Left err -> putStrLn (parseErrorPretty err)
            Right v  -> print (calculate v)

calculate = id

expr :: Parsec Dec Text Expression
expr = makeExprParser term table <?> "expression"

parens = between (char '(') (char ')')

term = parens expr 
    <|> lambda
    <|> ite
    <|> (Var <$> letterChar)
    <|> (Lit <$> boolLit) 
    <|> (Lit . I <$> L.decimal) 
    <?> "term"

ite = do 
    sym "if"
    c <- parens (expr)
    space *> sym "then"
    t <- expr
    space *> sym "else"
    e <- expr
    return (Ite c t e)

lambda = do 
    sym "\\" <|> sym "l" 
    v <- letterChar <?> "variable"
    space *> sym "->"
    e <- expr
    return (Lam v e)

boolLit = (B True <$ sym "True") 
          <|> (B False <$ sym "False")

type Expression = Fix Expression'

data Expression' r = Lit' Val
                | Var' Char
                | Negate' r
                | Add' r r
                | Sub' r r
                | Mul' r r
                | Div' r r
                | Lam' Char r
                | Ite' r r r
                deriving (Eq,Show,Ord,Functor,Foldable,Traversable)

pattern Lit v = Fix (Lit' v)
pattern Var c = Fix (Var' c)
pattern Negate r = Fix (Negate' r)
pattern Add r r' = Fix (Add' r r')
pattern Sub r r' = Fix (Sub' r r')
pattern Mul r r' = Fix (Mul' r r')
pattern Div r r' = Fix (Div' r r')
pattern Lam c r = Fix (Lam' c r)
pattern Ite r r' r'' = Fix (Ite' r r' r'')


data Val = B Bool | I Integer deriving (Eq,Show,Ord)

{-
data Expression = Lit Val
                | Var Char
                | Negate Expression 
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Lam Char Expression
                | Ite Expression Expression Expression
                deriving (Eq,Show,Ord)
-}

table = [ [ prefix  "-"  Negate
          , prefix  "+"  id ]
        , [ binary  "*"  Mul
          , binary  "/"  Div  ]
        , [ binary  "+"  Add
          , binary  "-"  Sub  ] ]

sym n = string n <* space

binary  name f = InfixL  (f <$ sym name)
prefix  name f = Prefix  (f <$ sym name)
postfix name f = Postfix (f <$ sym name)
