module Lam.Semantics where

import           Data.List  (delete, nub)
import           Lam.Syntax

-- reduce to normal form with number of reductions
multiStep :: Expr -> ([(Integer, Expr)]) -> (([(Integer, Expr)]), Expr, Int)
multiStep t mem =
  case step t mem of
    Just (t', mem) -> let (mem', t'', n) = multiStep t' mem in (mem', t'', n + 1)
    Nothing        -> (mem, t, 0)

updateDict :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
updateDict [] _ _ = []
updateDict ((x,y):xs) a b
  | a == x    = ((x,b) : xs) 
  | otherwise = ((x,y) : updateDict xs a b)

delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = filter (\a -> (fst a) /= key) l

-- single step reduction
step :: Expr -> [(Integer, Expr)] -> Maybe (Expr,[(Integer, Expr)])
step (App (Var "free") (Ref n)) mem = Just (Unit, delFromAL mem n)
step (App (Var "alloc") t) mem =
  let n = maxPointer mem in Just (Ref (n + 1), ((n + 1, t) : mem))
step (App (App (Var "swap") t) (Ref n)) mem =
  case lookup n mem of
    Just t' ->
      case updateDict mem n t of
        []   -> Nothing
        mem' -> Just ((Pair (t') (Ref n)), mem')
    Nothing -> Nothing
-- beta rule for function types
step (App (Abs x _ t) t') mem = Just ((subst t t' x), mem)
-- congruence rule for abs.
step (Abs x mty t) mem =
  case step t mem of
    Just (t', mem) -> Just ((Abs x mty t'), mem)
    Nothing -> Nothing
-- congruence rule for app.
step (App t1 t2) mem =
  case step t1 mem of
    Just (t1', mem) -> Just ((App t1' t2), mem)
    Nothing  ->
      case step t2 mem of
        Just (t2', mem) -> Just ((App t1 t2'), mem)
        Nothing  -> Nothing
-- congruence rule for pairs
step (Pair t1 t2) mem =
  case step t1 mem of
    Just (t1', mem) -> Just ((Pair t1' t2), mem)
    Nothing  ->
      case step t2 mem of
        Just (t2', mem) -> Just ((Pair t1 t2'), mem)
        Nothing -> Nothing
-- beta rule for pair types
step (LetPair (x, y) (Pair e1 e2) t) mem = Just ((subst (subst t e1 x) e2 y), mem)
-- congruence rule for let pair
step (LetPair p e t) mem =
  case step e mem of
    Just (e', mem) -> Just ((LetPair p e' t), mem)
    Nothing ->
      case step t mem of 
        Just (t', mem) -> Just ((LetPair p e t'), mem)
        Nothing -> Nothing
-- beta rule for LetUnit
step (LetUnit Unit t) mem = Just (t, mem)
-- congruence rule for LetUnit
step (LetUnit t1 t2) mem  =
  case step t1 mem of
    Just (t1', mem) -> Just ((LetUnit t1' t2), mem)
    Nothing ->
      case step t2 mem of
        Just (t2', mem) -> Just ((LetUnit t1 t2'), mem)
        Nothing -> Nothing
step t _ = Nothing

maxPointer :: [(Integer, Expr)] -> Integer
maxPointer []  = 0
maxPointer mem = let (pointers, _) = unzip mem in maximum pointers

-- subst t t' x
-- replace all occurences of x in t with t'
subst :: Expr -> Expr -> Identifier -> Expr
subst (Var y) t' x | y == x    = t'
                   | otherwise = Var y
subst (App t1 t2) t' x         = App (subst t1 t' x) (subst t2 t' x)
subst (Abs y mty t) t' x =
  if x == y
    then Abs y mty t
    else
      if y `elem` freeVars t'
        then
          let freshVar = y ++ "'"
              freshened = subst t (Var freshVar) y
          in Abs freshVar mty (subst freshened t' x)
        else Abs y mty (subst t t' x)
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
subst Unit _ _    = Unit
subst (LetUnit t1 t2) t' x = LetUnit (subst t1 t' x) (subst t2 t' x)
subst (Ref n) _ _ = Ref n
                                    
freeVars :: Expr -> [Identifier]
freeVars (Var x)          = [x]
freeVars (App t1 t2)      = freeVars t1 ++ freeVars t2
freeVars (Abs x _ t)      = delete x (nub (freeVars t))
freeVars (Pair t1 t2)     = freeVars t1 ++ freeVars t2
freeVars (LetPair _ t t') = freeVars t ++ freeVars t'
freeVars Unit             = []
freeVars (LetUnit t1 t2)  = freeVars t1 ++ freeVars t2
freeVars (Ref _)          = []
