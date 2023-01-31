{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Lam.Syntax where

type Identifier = String

data Expr =
          Var Identifier
        | App Expr Expr
        | Abs Identifier Expr
        deriving Show
