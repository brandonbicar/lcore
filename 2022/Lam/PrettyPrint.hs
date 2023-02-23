{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Lam.PrettyPrint where

import Lam.Syntax

-- Pretty print terms
class PrettyPrint t where
    isLexicallyAtomic :: t -> Bool
    isLexicallyAtomic _ = False

    pprint :: t -> String

bracket_pprint :: PrettyPrint t => t -> String
bracket_pprint t | isLexicallyAtomic t = pprint t
                 | otherwise           = "(" ++ pprint t ++ ")"

-- Untyped lambda calculus
instance PrettyPrint Expr where
    isLexicallyAtomic (Var _)     = True
    isLexicallyAtomic _           = False

    pprint (Abs var e)  = "\\" ++ var ++ " -> " ++ pprint e ++ ""
    pprint (App e1 e2)  = bracket_pprint e1 ++ " " ++ bracket_pprint e2
    pprint (Var var)    = var
    pprint (Pair e1 e2) = "<" ++ pprint e1 ++ ", " ++ pprint e2 ++ ">"
    pprint (LetPair (x, y) t t') =  "let <" ++ x ++ ", " ++ y ++ "> = " ++ pprint t ++ "in " ++ pprint t'
