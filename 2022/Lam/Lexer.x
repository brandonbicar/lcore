{
{-# OPTIONS_GHC -w #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Lam.Lexer (Token(..),scanTokens,symString
                 ,getPos) where

import Data.Text (Text)
import GHC.Generics (Generic)

}

%wrapper "posn"

$digit  = 0-9
$alpha  = [a-zA-Z\_\-\=]
$lower  = [a-z]
$upper  = [A-Z]
$eol    = [\n]
$alphanum  = [$alpha $digit \_]
@constr = ($upper ($alphanum | \')* | \(\))
@sym    = $lower ($alphanum | \')*
@int    = \-? $digit+
@charLiteral = \' ([\\.]|[^\']| . ) \'
@stringLiteral = \"(\\.|[^\"]|\n)*\"

@langPrag = [a-z]+

tokens :-

  $white*$eol                   { \p s -> TokenNL p }
  $eol+                         { \p s -> TokenNL p }
  $white+                       ;
  "--".*                        ;
  @constr			{ \p s -> TokenConstr p s }
  let                           { \p s -> TokenLet p }
  in                            { \p s -> TokenIn p }
  @sym				{ \p s -> TokenSym p s }
  "->"                          { \p s -> TokenArrow p }
  ":"                           { \p s -> TokenColon p }
  \\                            { \p s -> TokenLambda p }
  \=                            { \p s -> TokenEq p }
  \(                            { \p s -> TokenLParen p }
  \)                            { \p s -> TokenRParen p }
  \<                            { \p s -> TokenLPair p }
  \>                            { \p s -> TokenRPair p }
  \,                            { \p s -> TokenComma p }
  \*                            { \p s -> TokenStar p }
  
{

data Token
  = TokenLambda   { posn :: AlexPosn }
  | TokenConstr   { posn :: AlexPosn, constr :: String }
  | TokenSym      { posn :: AlexPosn, sym :: String }
  | TokenEq       { posn :: AlexPosn }
  | TokenArrow    { posn :: AlexPosn }
  | TokenLParen   { posn :: AlexPosn }
  | TokenRParen   { posn :: AlexPosn }
  | TokenNL       { posn :: AlexPosn }
  | TokenLet      { posn :: AlexPosn }
  | TokenIn       { posn :: AlexPosn }
  | TokenLPair    { posn :: AlexPosn }
  | TokenRPair    { posn :: AlexPosn }
  | TokenComma    { posn :: AlexPosn }
  | TokenColon    { posn :: AlexPosn }
  | TokenStar     { posn :: AlexPosn }
  deriving (Eq, Show, Generic)

symString :: Token -> String
symString (TokenSym _ x) = x
symString _ = error "Not a symbol"

scanTokens = alexScanTokens >>= (return . trim)

trim :: [Token] -> [Token]
trim = reverse . trimNL . reverse . trimNL

trimNL :: [Token] -> [Token]
trimNL [] = []
trimNL (TokenNL _ : ts) = trimNL ts
trimNL ts = ts

getPos :: Token -> (Int, Int)
getPos t = (l, c)
  where (AlexPn _ l c) = posn t

}
