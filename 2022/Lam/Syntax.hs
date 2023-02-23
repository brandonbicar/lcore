{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Lam.Syntax where

type Identifier = String

data Expr =
          Var Identifier
        | App Expr Expr
        | Abs Identifier Expr
        | Pair Expr Expr
        | LetPair (Identifier, Identifier) Expr Expr
        deriving Show
