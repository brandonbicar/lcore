{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Lam.Syntax where

type Identifier = String

data Expr =
          Var Identifier
        | App Expr Expr
        | Abs Identifier (Maybe Type) Expr
        | Pair Expr Expr
        | LetPair (Identifier, Identifier) Expr Expr
        deriving Show

type Name = String

data Type = FunTy Type Type
          | Cons Name
  deriving (Show, Eq)

type Context = [(Identifier, Type)]

check :: Context -> Expr -> Type -> Bool
check = error "TODO"
