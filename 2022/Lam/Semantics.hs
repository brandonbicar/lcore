module Lam.Semantics where

import           Data.List  (delete, nub)
import           Lam.Syntax

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
-- beta rule for function types
step (App (Abs x t) t') = Just (subst t t' x)
-- congruence rule for abs.
step (Abs x t) =
  case step t of
    Just t' -> Just (Abs x t')
    Nothing -> Nothing
-- congruence rule for app.
step (App t1 t2) =
  case step t1 of
    Just t1' -> Just (App t1' t2)
    Nothing  ->
      case step t2 of
        Just t2' -> Just (App t1 t2')
        Nothing  -> Nothing
-- congruence rule for pairs
step (Pair t1 t2) =
  case step t1 of
    Just t1' -> Just (Pair t1' t2)
    Nothing  ->
      case step t2 of
        Just t2' -> Just (Pair t1 t2')
        Nothing -> Nothing
-- beta rule for pair types
step (LetPair (x, y) (Pair e1 e2) t) = Just (subst (subst t e1 x) e2 y)
-- congruence rule for let pair
step (LetPair p e t) =
  case step e of
    Just e' -> Just (LetPair p e' t)
    Nothing ->
      case step t of 
        Just t' -> Just (LetPair p e t')
        Nothing -> Nothing
step t = Nothing


subst :: Expr -> Expr -> Identifier -> Expr
subst (Var y) t' x | y == x    = t'
                   | otherwise = Var y
subst (App t1 t2) t' x         = App (subst t1 t' x) (subst t2 t' x)
subst (Abs y t) t' x =
  if x == y
    then Abs y t
    else
      if y `elem` freeVars t'
        then
          let freshVar = y ++ "'"
              freshened = subst t (Var freshVar) y
          in Abs freshVar (subst freshened t' x)
        else Abs y (subst t t' x)
subst (Pair e1 e2) t' x = Pair (subst e1 t' x) (subst e2 t' x)
subst (LetPair (x, y) t t') e i = LetPair (x, y) (subst t e i) (subst t' e i)
-- TODO: freshen vars to avoid variable capture
--  if x == i || y == i
--    then LetPair (x, y) t t'
--  else
--    if x `elem` freeVars e
--      then 
--        let freshX = x ++ "'" 
--            freshened = subst t (Var freshX) x
--        in LetPair (x, y) (subst 
--               if y `elem` freeVars e
--                 then let freshY = y ++ "'"
                                    
freeVars :: Expr -> [Identifier]
freeVars (Var x)     = [x]
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2
freeVars (Abs x t)   = delete x (nub (freeVars t))
freeVars (Pair t1 t2) = freeVars t1 ++ freeVars t2
freeVars (LetPair _ t t') = freeVars t ++ freeVars t'
