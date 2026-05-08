module Eval where

import Syntax

lookupEnv :: Name -> Env -> Either String Value
lookupEnv x [] = Left ("Unbound variable: " ++ x)
lookupEnv x ((y, v):env)
  | x == y    = Right v
  | otherwise = lookupEnv x env

eval :: Env -> Expr -> Either String Value
eval _   (EInt n) = Right (VInt n)
eval _   (EBool b) = Right (VBool b)

eval env (EVar x) =
  lookupEnv x env

eval env (EIf cond eThen eElse) = do
  vCond <- eval env cond
  case vCond of
    VBool True  -> eval env eThen
    VBool False -> eval env eElse
    _ -> Left "Condition in if must be a boolean"

eval env (ELet x e1 e2) = do
  v1 <- eval env e1
  eval ((x, v1) : env) e2

eval env (EAdd e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right (VInt (n1 + n2))
    _ -> Left "Both arguments of + must be integers"

eval env (ESub e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right (VInt (n1 - n2))
    _ -> Left "Both arguments of - must be integers"


eval env (ELetRec f _ e1 e2) =
  case e1 of
    ELam x t body ->
      let recClosure = VClosure x body recEnv
          recEnv = (f, recClosure) : env
      in eval recEnv e2
    _ -> Left "letrec requires a function on the right-hand side"

eval env (EMul e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right (VInt (n1 * n2))
    _ -> Left "Both arguments of * must be integers"

eval env (EEq e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2)   -> Right (VBool (n1 == n2))
    (VBool b1, VBool b2) -> Right (VBool (b1 == b2))
    _ -> Left "Arguments of == must both be Int or both be Bool"

eval env (ELt e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right (VBool (n1 < n2))
    _ -> Left "Both arguments of < must be integers"

eval env (ELam x _ body) =
  Right (VClosure x body env)

eval env (EApp e1 e2) = do
  funVal <- eval env e1
  argVal <- eval env e2
  case funVal of
    VClosure x body closureEnv ->
      eval ((x, argVal) : closureEnv) body
    _ -> Left "Attempted to apply a non-function"

eval _ (ENil _) =
  Right VNil

eval env (ECons eHead eTail) = do
  vHead <- eval env eHead
  vTail <- eval env eTail
  case vTail of
    VNil -> Right (VCons vHead VNil)
    VCons _ _ -> Right (VCons vHead vTail)
    _ -> Left "Second argument of Cons must be a list"

eval env (ECase e branches) = do
  v <- eval env e
  evalCase env v branches

evalCase :: Env -> Value -> [(Pattern, Expr)] -> Either String Value
evalCase _ _ [] =
  Left "Non-exhaustive patterns in case expression"

evalCase env v ((pat, expr):rest) =
  case matchPattern pat v of
    Just bindings -> eval (bindings ++ env) expr
    Nothing       -> evalCase env v rest

matchPattern :: Pattern -> Value -> Maybe Env
matchPattern PNil VNil = Just []
matchPattern (PCons x xs) (VCons vHead vTail) =
  Just [(x, vHead), (xs, vTail)]
matchPattern _ _ = Nothing