module Lam.LinearTypes where

import           Lam.Syntax
import           Lam.PrettyPrint
import           Data.List (intersect)


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

commonVars :: Context -> Context -> [Identifier]
commonVars c1 c2 = intersect (map fst c1) (map fst c2)

-- temporary, should move to PrettyPrinter.hs
pprintVars :: [Identifier] -> String
pprintVars [] = []
pprintVars (var:rest) = var ++ " " ++ pprintVars rest

delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = filter (\a -> (fst a) /= key) l

check :: Context -> Expr -> Type -> Either TypeError (Bool, Context)
check gamma (Abs x _ t) (FunTy tyA tyB) =
  case check ((x, tyA):gamma) t tyB of
    Right (b, delta) ->
      case (elem x (map fst delta)) of
        True  -> Right (b, (delFromAL delta x))
        False -> Left $ "Linear type error: variable " ++ x ++ " was not used."
    Left err -> Left err
check gamma e t =
  case synth gamma e of
    Right (t', delta) -> Right $ ((t == t'), delta)
    Left error        -> Left error

synth :: Context -> Expr -> Either TypeError (Type, Context)
synth gamma (Unit) = Right (UnitTy, [])

synth gamma (App (Var "alloc") t) =
  case synth gamma t of
    Right (ty, delta) -> Right (RefTy ty, delta)
    Left err          -> Left err

synth gamma (App (App (Var "swap") t1) t2) =
  case synth gamma t1 of
    Right (ty, delta1) ->
      case check gamma t2 (RefTy ty) of
        Right (True, delta2)  ->
          case commonVars delta1 delta2 of
            []   -> Right ((PairTy ty (RefTy ty)), (delta1 ++ delta2))
            vars -> Left $ "linear type error: some variable is used more than once - " ++ pprintVars vars
        Right (False, _)      -> Left $ "In use of swap, expected type " ++ pprint (RefTy ty)
        Left err              -> Left err
    Left err -> Left err

synth gamma (App (Var "free") t) =
  case synth gamma t of
    Right ((RefTy ty), delta) -> Right (UnitTy, delta)
    Right (ty, _)             -> Left $ pprint ty ++ " is not a reference"
    Left err                  -> Left err

synth gamma (Var x) =
  case lookup x gamma of
    Just ty -> Right (ty, [(x, ty)])
    Nothing -> Left $ "I don't know the type of free variable " ++ x

synth gamma (Abs x (Just tyA) t) =
  case synth ((x, tyA):gamma) t of
    Right (tyB, delta) -> Right ((FunTy tyA tyB), delta)
    Left  err          -> Left err

synth gamma (App t1 t2) =
  case synth gamma t1 of
    Right ((FunTy tyA tyB), delta1) ->
      case check gamma t2 tyA of
        Right (True, delta2) ->
          case commonVars delta1 delta2 of
            []   -> Right (tyB, (delta1 ++ delta2)) 
            vars -> Left $ "linear type error: some variable is used more than once - " ++ pprintVars vars
        Right (False, _)     -> Left $ "Expected type " ++ pprint tyA
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
    Right (tyA, delta1) ->
      case synth gamma t2 of
        Right (tyB, delta2) ->
          case commonVars delta1 delta2 of
            []   -> Right ((PairTy tyA tyB), (delta1 ++ delta2))
            vars -> Left $ "linear type error: some variable is used more than once - " ++ pprintVars vars
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
    Right ((PairTy tyA tyB), delta1) ->
      case synth ((x, tyA):(y, tyB):gamma) t2 of
        Right (tyC, delta2) ->
          case commonVars delta1 delta2 of
            []   -> Right (tyC, (delta1 ++ delta2))
            vars -> Left $ "linear type error: some variable is used more than once - " ++ pprintVars vars
        Left err            -> Left err
    Right _ -> Left $ "Left hand side of application " ++ pprint t1 ++ " is not a pair"
    Left err -> Left err


synth _ t = Left $ "Cannot infer type of " ++ pprint t ++ ". Add more type signatures."

