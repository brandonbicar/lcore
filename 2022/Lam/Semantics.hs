module Lam.Semantics where

import Lam.Syntax
import Data.List (nub, delete)

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
                     | otherwise = if y `elem` freeVars t'
                                     then 
                                       let freshVar = y ++ "'"
                                           freshened = subst t (Var freshVar) y
                                       in Abs freshVar (subst freshened t' x)
                                     else Abs y (subst t t' x)

freeVars :: Expr -> [Identifier]
freeVars (Var x)     = [x]
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2
freeVars (Abs x t)   = delete x (nub (freeVars t))


-- (\x -> \y -> x) (y a)
-- \y' -> y a
--
-- multiStep App (Abs "x" (Abs "y" (Var "x"))) (App (Var "y") (Var "a"))
--
-- step App (Abs "x" (Abs "y" (Var "x"))) (App (Var "y") (Var "a"))
--
-- x = "x", t = (Abs "y" (Var "x")), t' = (App (Var "y") (Var "a"))
--
-- Just (subst (Abs "y" (Var "x")) (App (Var "y") (Var "a")) "x")
--
-- y = "y", t = (Var "x"), t' = (App (Var "y") (Var "a")), x = "x"
--
-- Abs "y" (subst (Var "x") (App (Var "y") (Var "a")) ("x"))
--
-- y = "x", t' = (App (Var "y") (Var "a")), x = "x"
--
-- (App (Var "y") (Var "a"))
--
-- Just (Abs "y'" (App (Var "y") (Var "a")))
