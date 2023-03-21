module Lam.Types where

import           Lam.Syntax
import           Lam.PrettyPrint

-- Types
{-

*************************************************************
Declarative specification of the simply-typed lambda calculus
*************************************************************
Recall contexts are like lists of variable-type assumptions

G ::=  G, x : A | .

       (x : A) in G
var ----------------------
       G |- x : A

     G |- e1 : A -> B      G |- e2 : A
app ---------------------------------------
    G |- e1 e2 : B

      G, x : A |- e : B
abs ------------------------
      G |- \x -> e : A -> B

-}

check :: Context -> Expr -> Type -> Bool
check gamma (Abs x _ t) (FunTy tyA tyB) =
  check ((x, tyA):gamma) t tyB
check gamma e t =
  case synth gamma e of
    Just t' -> t == t'
    Nothing -> False


synth :: Context -> Expr -> Maybe Type
synth gamma (Var x) = lookup x gamma

synth gamma (Abs x (Just tyA) t) =
  case synth ((x, tyA):gamma) t of
    Just tyB -> Just (FunTy tyA tyB)
    Nothing  -> Nothing

synth gamma (App t1 t2) =
  case synth gamma t1 of
    Just (FunTy tyA tyB) ->
      if check gamma t2 tyA
        then Just tyB
        else error $ "Expected type " ++ pprint tyA
    Just _  -> error $ "Left hand side of application " ++ pprint t1 ++ " is not a function"
    Nothing -> Nothing

synth _ t = error $ "Cannot infer type of " ++ pprint t

