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
      G |- \(x : A) -> e : A -> B

-}

type TypeError = String

check :: Context -> Expr -> Type -> Either TypeError Bool
check gamma (Abs x _ t) (FunTy tyA tyB) =
  check ((x, tyA):gamma) t tyB
check gamma e t =
  case synth gamma e of
    Right t'   -> Right $ t == t'
    Left error -> Left error


synth :: Context -> Expr -> Either TypeError Type
synth gamma (Unit) = Right UnitTy

synth gamma (App (Var "alloc") t) =
  case synth gamma t of
    Right ty  -> Right (RefTy ty)
    Left err  -> Left err

synth gamma (App (App (Var "swap") t1) t2) =
  case synth gamma t1 of
    Right ty ->
      case check gamma t2 (RefTy ty) of
        Right True  -> Right (PairTy ty (RefTy ty))
        Right False -> Left $ "In use of swap, expected type " ++ pprint (RefTy ty)
        Left err    -> Left err
    Left err -> Left err

synth gamma (App (Var "free") t) =
  case synth gamma t of
    Right (RefTy ty) -> Right UnitTy
    Right ty         -> Left $ pprint ty ++ " is not a reference"
    Left err         -> Left err

synth gamma (Var x) =
  case lookup x gamma of
    Just ty -> Right ty
    Nothing -> Left $ "I don't know the type of free variable " ++ x

synth gamma (Abs x (Just tyA) t) =
  case synth ((x, tyA):gamma) t of
    Right tyB -> Right (FunTy tyA tyB)
    Left  err -> Left err

synth gamma (App t1 t2) =
  case synth gamma t1 of
    Right (FunTy tyA tyB) ->
      case check gamma t2 tyA of
        Right True  -> Right tyB
        Right False -> Left $ "Expected type " ++ pprint tyA
        Left err    -> Left err
    Right _  -> Left $ "Left hand side of application " ++ pprint t1 ++ " is not a function"
    Left err -> Left err

{-
      G |- t1 : A
      G |- t2 : B
pair -------------------------
      G |- (t1,t2) : A * B
-}

synth gamma (Pair t1 t2) =
  case synth gamma t1 of
    Right tyA ->
      case synth gamma t2 of
        Right tyB -> Right (PairTy tyA tyB)
        Left err -> Left err
    Left err -> Left err

{-
        G |- t1 : A * B
        G |- x : A, y : B |- t2 : C
letpair -------------------------
        G |- let <x,y> = t1 in t2 : C
-}


synth gamma ((LetPair (x,y) (t1) (t2))) =
  case synth gamma t1 of
    Right (PairTy tyA tyB) ->
      case synth ((x, tyA):(y, tyB):gamma) t2 of
        Right tyC -> Right tyC
        Left err -> Left err
    Right _ -> Left $ "Left hand side of application " ++ pprint t1 ++ " is not a pair"
    Left err -> Left err


synth _ t = Left $ "Cannot infer type of " ++ pprint t ++ ". Add more type signatures."

