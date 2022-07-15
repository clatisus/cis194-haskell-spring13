{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char (isSpace, isAlpha, isAlphaNum)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- λ> runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
-- Just ("ABC","dEfgH")
-- λ> runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
-- Just ("ABC","dEfgH")
-- λ> runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh"
-- Just ("","abcdeFGh")
-- λ> runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"
-- Nothing

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
-- spaces = ((:) <$> satisfy isSpace <*> spaces) <|> pure []
spaces = zeroOrMore $ satisfy isSpace

-- λ> runParser spaces "abcd"
-- Just ("","abcd")
-- λ> runParser spaces "    abcd"
-- Just ("    ","abcd")

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

-- λ> runParser ident "foobar baz"
-- Just ("foobar"," baz")
-- λ> runParser ident "foo33fA"
-- Just ("foo33fA","")
-- λ> runParser ident "2bad"
-- Nothing
-- λ> runParser ident ""
-- Nothing

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser Atom
parseAtom = (N <$> posInt) <|> (I <$> ident)

parseSExpr :: Parser SExpr
parseSExpr = spaces *>
               ((A <$> parseAtom)
                <|>
                (char '(' *> (Comb <$> zeroOrMore parseSExpr) <* char ')'))
             <* spaces

-- λ> runParser parseSExpr "5"
-- Just (A (N 5),"")
-- λ> runParser parseSExpr "foo3"
-- Just (A (I "foo3"),"")
-- λ> runParser parseSExpr "(bar (foo) 3 5 874)"
-- Just (Comb [A (I "bar"),Comb [A (I "foo")],A (N 3),A (N 5),A (N 874)],"")
-- λ> runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
-- Just (Comb [Comb [Comb [A (I "lambda"),A (I "x"),Comb [A (I "lambda"),A (I "y"),Comb [A (I "plus"),A (I "x"),A (I "y")]]],A (N 3)],A (N 5)],"")
-- λ> runParser parseSExpr "(   lots  of   (  spaces   in  )  this ( one ) )"
-- Just (Comb [A (I "lots"),A (I "of"),Comb [A (I "spaces"),A (I "in")],A (I "this"),Comb [A (I "one")]],"")
