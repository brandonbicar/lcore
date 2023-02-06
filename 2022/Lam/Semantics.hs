module Lam.Semantics where

import Lam.Syntax

-- reduce to normal form with number of reductions
multiStep :: Expr -> (Expr, Int)
multiStep t =
        case step t of
          Nothing -> (t, 0)
          Just t' -> let (t'', n) = multiStep t' in (t'', n + 1)

-- single step reduction
-- step :: Expr
step :: Expr -> Maybe Expr
-- step (App (Var "read") t) =
step (App (Abs x t) t') = Just (subst t t' x) 
step (App t1 t2) =
  case step t1 of
    Just t1' -> Just (App t1' t2)
    Nothing  -> Nothing
step t = Nothing


subst :: Expr -> Expr -> Identifier -> Expr
subst (Var y) t' x | y == x    = t'
                   | otherwise = Var y
subst (App t1 t2) t' x         = App (subst t1 t' x) (subst t2 t' x)
subst (Abs y t) t' x | x == y    = Abs y t
                     | otherwise = Abs y (subst t t' x)
