module Eval where

import Lexer (Token(..), TokenType(..), lexer, showToken, showTokens)
import Data.Int (Int64)
import Control.Monad (foldM)

ok f a b = Right (f a b)

idivide _ 0 = Left "division by zero"
idivide a b = Right (a `div` b)

fdivide _ 0 = Left "division by zero"
fdivide a b = Right (a / b)

applyOpInt64 :: String -> Int64 -> Int64 -> Either String Int64
applyOpInt64 "+" = ok (+)
applyOpInt64 "-" = ok (-)
applyOpInt64 "*" = ok (*)
applyOpInt64 "/" = idivide
applyOpInt64 op  = const (const (Left ("unknown operator: " ++ op)))

applyOpFloat64 :: String -> Double -> Double -> Either String Double
applyOpFloat64 "+" = ok (+)
applyOpFloat64 "-" = ok (-)
applyOpFloat64 "*" = ok (*)
applyOpFloat64 "/" = fdivide
applyOpFloat64 op  = const (const (Left ("unknown operator: " ++ op)))

evalRPN
  :: (TokenType -> Either String a)
  -> (String -> a -> a -> Either String a)
  -> (a -> TokenType)
  -> [Token]
  -> Either String Token
evalRPN readLit apply wrap toks = do
    stack <- foldM step [] toks
    case stack of
      [v] ->
        let pos = maybe (tokenPos (head toks)) tokenPos (safeLast toks)
        in Right (Token (wrap v) pos "")

      _ -> Left "incorrect RPN expression"
  where
    step stack tok =
      case tokenType tok of
        TokStdop op ->
          case stack of
            b:a:rest -> do
              r <- apply op a b
              pure (r : rest)
            _ -> Left "stack underflow"

        tt -> do
          v <- readLit tt
          pure (v : stack)

    safeLast [] = Nothing
    safeLast xs = Just (last xs)

evalTokensToInt64 :: [Token] -> Either String Token
evalTokensToInt64 = evalRPN read applyOpInt64 (TokIntLit . toInteger)
  where
    read (TokIntLit n)   = Right (fromInteger n)
    read (TokFloatLit _) = Left "float literal not allowed in int64 evaluation"
    read _               = Left "unsupported token"

evalTokensToFloat64 :: [Token] -> Either String Token
evalTokensToFloat64 = evalRPN read applyOpFloat64 TokFloatLit
  where
    read (TokFloatLit n) = Right n
    read (TokIntLit n)   = Right (fromInteger n)
    read _               = Left "unsupported token"
