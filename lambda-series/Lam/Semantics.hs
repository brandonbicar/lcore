{-# LANGUAGE FlexibleInstances #-}

module Lam.Semantics where

import Lam.Syntax
import Lam.Options

import qualified Data.Set as Set

-- Keep doing small step reductions until normal form reached
multiStep :: [Option] -> Expr PCF -> (Expr PCF, Int)
multiStep opts e | isCBV opts = multiStep' callByValue e 0
multiStep opts e | isCBN opts = multiStep' callByName e 0
multiStep _    e              = multiStep' fullBeta e 0

type Reducer a = a -> Maybe a

multiStep' :: Reducer (Expr PCF) -> Expr PCF -> Int -> (Expr PCF, Int)
multiStep' step t n =
  case step t of
    -- Normal form reached
    Nothing -> (t, n)
    -- Can do more reduction
    Just t' -> multiStep' step t' (n+1)

fullBeta :: Reducer (Expr PCF)
fullBeta (Var _) = Nothing
fullBeta (App (Abs x e) e') = beta e x e'
-- Poly beta
fullBeta (App (TyAbs var e) (TyEmbed t)) = beta e var (TyEmbed t)
fullBeta (App e1 e2) =
  -- Prefer fully zeta1 reducing before zeta2 reducing
  case zeta1 fullBeta e1 e2 of
    Just e -> Just e
    Nothing -> zeta2 fullBeta e1 e2
fullBeta (Abs x e) = zeta3 fullBeta x e
fullBeta (Sig e _) = Just e
fullBeta (Ext e) = reducePCF fullBeta (Ext e)
-- Poly
fullBeta (TyAbs x e) = zeta3Ty fullBeta x e
fullBeta (TyEmbed t) = Nothing


callByName :: Reducer (Expr PCF)
callByName (Var _) = Nothing
callByName (App (Abs x e) e') = beta e x e'
-- Poly beta
callByName (App (TyAbs var e) (TyEmbed t)) = beta e var (TyEmbed t)
callByName (App e1 e2) = zeta1 callByName e1 e2
callByName (Abs x e) = Nothing
callByName (Sig e _) = Just e
callByName (Ext e) = reducePCF callByName (Ext e)
-- Poly
callByName (TyAbs x e) = Nothing
callByName (TyEmbed t) = Nothing

callByValue :: Reducer (Expr PCF)
callByValue (Var _) = Nothing
callByValue (App (Abs x e) e') | isValue e' = beta e x e'
-- Poly beta
callByValue (App (TyAbs var e) (TyEmbed t)) = beta e var (TyEmbed t)
callByValue (App e1 e2) | isValue e1 = zeta2 callByValue e1 e2
callByValue (App e1 e2) = zeta1 callByValue e1 e2
callByValue (Abs x e) = Nothing
callByValue (Sig e _) = Just e
callByValue (Ext e) = reducePCF callByValue (Ext e)
-- Poly
callByValue (TyAbs x e) = Nothing
callByValue (TyEmbed t) = Nothing

-- Base case
beta :: (Substitutable t) => t -> Identifier -> t -> Maybe t
beta e x e' = Just (substitute e (x, e'))

-- Inductive rules
zeta1 :: Reducer (Expr PCF) -> Expr PCF -> Expr PCF -> Maybe (Expr PCF)
zeta1 step e1 e2 = (\e1' -> App e1' e2) <$> step e1

zeta2 :: Reducer (Expr PCF)  -> Expr PCF -> Expr PCF -> Maybe (Expr PCF)
zeta2 step e1 e2 = (\e2' -> App e1 e2') <$> step e2

zeta3 :: Reducer (Expr PCF)  -> Identifier -> Expr PCF -> Maybe (Expr PCF)
zeta3 step x e = (\e' -> Abs x e') <$> step e

zeta3Ty :: Reducer (Expr PCF)  -> Identifier -> Expr PCF -> Maybe (Expr PCF)
zeta3Ty step x e = (\e' -> TyAbs x e') <$> step e


-- Reducer for the extended PCF syntax
reducePCF :: Reducer (Expr PCF) -> Reducer (Expr PCF)

-- Fix point
reducePCF step (Ext (Fix e)) = return $ App e (Ext $ Fix e)

-- Beta-rules for Nat
reducePCF step (Ext (NatCase (Ext Zero) e1 _)) = Just e1

reducePCF step (Ext (NatCase (App (Ext Succ) n) _ (x,e2))) = Just $ substitute e2 (x,n)

-- Congruence for Nat-eliminator
reducePCF step (Ext (NatCase e e1 (x,e2))) =
  (\e' -> Ext (NatCase e' e1 (x,e2))) <$> step e

-- Congruence for productor constructor
reducePCF step (Ext (Pair e1 e2)) =
  case step e1 of
    Just e1' -> Just $ Ext $ Pair e1' e2
    Nothing -> (\e2' -> Ext $ Pair e1 e2') <$> step e2

-- Beta-rules for products
reducePCF step (Ext (Fst (Ext (Pair e1 e2)))) = Just e1
reducePCF step (Ext (Snd (Ext (Pair e1 e2)))) = Just e2

-- Congruence rules for product eliminators
reducePCF step (Ext (Fst e)) = (\e' -> Ext $ Fst e') <$> step e
reducePCF step (Ext (Snd e)) = (\e' -> Ext $ Snd e') <$> step e

-- Beta-rules for sum types
reducePCF step (Ext (Case (Ext (Inl e)) (x,e1) _)) = Just $ substitute e1 (x,e)
reducePCF step (Ext (Case (Ext (Inr e)) _ (y,e2))) = Just $ substitute e2 (y,e)

-- Congruence for sum eliminator
reducePCF step (Ext (Case e (x,e1) (y,e2))) =
  (\e' -> Ext (Case e' (x,e1) (y,e2))) <$> step e

-- Congruence for sum constructor
reducePCF step (Ext (Inl e)) = (\e' -> Ext $ Inl e') <$> step e
reducePCF step (Ext (Inr e)) = (\e' -> Ext $ Inr e') <$> step e

-- other Ext terms
reducePCF step (Ext _) = Nothing

-- Non Ext terms
reducePCF _ _ = error "invalid term"

class Substitutable e where
  substitute :: e -> (Identifier, e) -> e

instance Substitutable (Expr PCF) where
  substitute = substituteExpr

-- Syntactic substitution - `substituteExpr e (x, e')` means e[e'/x]
substituteExpr :: Expr PCF -> (Identifier, Expr PCF) -> Expr PCF
substituteExpr (Var y) (x, e')
  | x == y = e'
  | otherwise = Var y

substituteExpr (App e1 e2) s =
  App (substituteExpr e1 s) (substituteExpr e2 s)

substituteExpr (Abs y e) s =
  let (y', e') = substitute_binding y e s in Abs y' e'

substituteExpr (Sig e t) s = Sig (substituteExpr e s) t

-- PCF terms
substituteExpr (Ext Zero) s = Ext Zero
substituteExpr (Ext Succ) s = Ext Succ

substituteExpr (Ext (Fix e)) s = Ext $ Fix $ substituteExpr e s

substituteExpr (Ext (NatCase e e1 (y,e2))) s =
  let e'  = substituteExpr e s
      e1' = substituteExpr e1 s
      (y', e2') = substitute_binding y e2 s
  in Ext $ NatCase e' e1' (y', e2')

substituteExpr (Ext (Pair e1 e2)) s =
  Ext $ Pair (substituteExpr e1 s) (substituteExpr e2 s)

substituteExpr (Ext (Fst e)) s = Ext $ Fst $ substituteExpr e s
substituteExpr (Ext (Snd e)) s = Ext $ Snd $ substituteExpr e s

substituteExpr (Ext (Case e (x,e1) (y,e2))) s =
  let e' = substituteExpr e s
      (x', e1') = substitute_binding x e1 s
      (y', e2') = substitute_binding y e2 s
  in Ext $ Case e' (x', e1') (y', e2')

substituteExpr (Ext (Inl e)) s = Ext $ Inl $ substituteExpr e s
substituteExpr (Ext (Inr e)) s = Ext $ Inr $ substituteExpr e s

-- Poly

-- Substitute inside types
substituteExpr (TyEmbed t) (var, TyEmbed t') =
  TyEmbed (substituteType t (var, t'))

substituteExpr (TyEmbed t) (var, _) =
    TyEmbed t

substituteExpr (TyAbs y e) s =
  TyAbs y (substituteExpr e s)


-- substitute_binding x e (y,e') substitutes e' into e for y,
-- but assumes e has just had binder x introduced
substitute_binding :: (Term t, Substitutable t) => Identifier -> t -> (Identifier, t) -> (Identifier, t)
substitute_binding x e (y,e')
  -- Name clash in binding - we are done
  | x == y = (x, e)
  -- If expression to be bound contains already bound variable
  | x `Set.member` freeVars e' =
    let x' = fresh_var x (freeVars e `Set.union` freeVars e')
    in (x', substitute (substitute e (x, mkVar x')) (y, e'))
  | otherwise = (x, substitute e (y,e'))

instance Substitutable Type where
    substitute = substituteType

substituteType :: Type -> (Identifier, Type) -> Type
substituteType (FunTy t1 t2) s =
  FunTy (substituteType t1 s) (substituteType t2 s)

substituteType NatTy s = NatTy
substituteType (ProdTy t1 t2) s =
  ProdTy (substituteType t1 s) (substituteType t2 s)
substituteType (SumTy t1 t2) s =
  SumTy (substituteType t1 s) (substituteType t2 s)

-- Actual substitution happening here
substituteType (TyVar var) (x, t)
  | var == x  = t
  | otherwise = TyVar var

substituteType (Forall var t) s =
  let (var', t') = substitute_binding var t s in Forall var' t'


