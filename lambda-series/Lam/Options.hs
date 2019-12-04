module Lam.Options where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)

------------------------------
-- Language options that `lcore` accepts in files

data Option = PCF | Typed | CBV | CBN | Poly
  deriving (Eq, Show)

-- Some helpers
isCBV :: [Option] -> Bool
isCBV options = elem CBV options

isCBN :: [Option] -> Bool
isCBN options = elem CBN options

isFullBeta :: [Option] -> Bool
isFullBeta options = not (isCBV options) && not (isCBN options)

isPCF :: [Option] -> Bool
isPCF options = elem PCF options

isTyped :: [Option] -> Bool
isTyped options = elem Typed options

isPoly :: [Option] -> Bool
isPoly options = elem Poly options

language :: [Option] -> String
language options = if isPCF options then "PCF" else "lambda"

-- Builds up a the language option list and checks for conflicting options
addOption :: Option -> [Option] -> ReaderT String (Either String) [Option]
addOption opt opts =
  case opt of
    CBV | isCBN opts -> lift $ Left "Cannot choose both CBV and CBN."
    CBN | isCBV opts -> lift $ Left "Cannot choose both CBN and CBV."
    _ -> return $ opt : opts

showReducer :: [Option] -> String
showReducer opts | isCBV opts      = "Call-By-Value"
showReducer opts | isCBN opts      = "Call-By-Name"
showReducer opts | isFullBeta opts = "Determinised full beta"
showReducer _ = "no reducer statisfied the options"
