{
{-# LANGUAGE FlexibleContexts #-}

module Lam.Parser where

import Numeric
import System.Exit
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)

import Lam.Lexer
import Lam.Syntax

}

%name program Program
%name expr Expr
%tokentype { Token }
%error { parseError }
%monad { ReaderT String (Either String) }

%token
    nl      { TokenNL _ }
    '\\'    { TokenLambda _ }
    '->'    { TokenArrow _ }
    VAR     { TokenSym _ _ }
    CONSTR  { TokenConstr _ _ }
    '='     { TokenEq _ }
    ':'     { TokenColon _ }
    '('     { TokenLParen _ }
    ')'     { TokenRParen _ }
    '<'     { TokenLPair _ }
    '>'     { TokenRPair _ }
    ','     { TokenComma _ }
    let     { TokenLet _ }
    in      { TokenIn _ }
    '*'     { TokenStar _ }



%right '->'
%%

Program :: { Expr }
  : Defs  { $1 }

Defs :: { Expr }
  : Def NL Defs           { $1 $3 }
  | Expr                  { $1 }

NL :: { () }
  : nl NL                     { }
  | nl                        { }

Def :: { Expr -> Expr }
  : VAR '=' Expr { \program -> App (Abs (symString $1) Nothing program) $3 }
  | VAR ':' Type '=' Expr { \program -> App (Abs (symString $1) (Just $3) program) $5 }

Expr :: { Expr }
  : '\\' VAR '->' Expr
    { Abs (symString $2) Nothing $4 }

  | '\\' '(' VAR ':' Type ')' '->' Expr
    { Abs (symString $3) (Just $5) $8 }

  | Juxt
    { $1 }
 | let '<' VAR ',' VAR '>' '=' Expr in Expr
   { LetPair (symString $3, symString $5) $8 $10 }
  | let '(' ')' '=' Expr in Expr
    { LetUnit $5 $7 }

Type :: { Type }
  : Type '->' Type            { FunTy $1 $3 }
  | Type '*' Type             { PairTy $1 $3 }
  | CONSTR                    { Cons (constr $1) }
  | '(' Type ')'              { $2 }
  | '(' ')'                   { UnitTy }

Juxt :: { Expr }
  : Juxt Atom                 { App $1 $2 }
  | Atom                      { $1 }

Atom :: { Expr }
  : '(' Expr ')'              { $2 }
  | VAR                       { Var $ symString $1 }
  | '<' Expr ',' Expr '>'     { Pair $2 $4 }
  | '(' ')'                   { Unit }

{

parseError :: [Token] -> ReaderT String (Either String) a
parseError [] = lift . Left $ "Premature end of file"
parseError t  =  do
    file <- ask
    lift . Left $ file <> ":" <> show l <> ":" <> show c
                        <> ": parse error"
  where (l, c) = getPos (head t)

parseProgram :: FilePath -> String -> Either String Expr
parseProgram file input = runReaderT (program $ scanTokens input) file

}
