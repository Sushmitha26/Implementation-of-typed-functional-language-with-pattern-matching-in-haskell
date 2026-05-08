module TypeCheck where

import Syntax

lookupType :: Name -> TypeEnv -> Either String Type
lookupType x [] = Left ("Unbound variable: " ++ x)
lookupType x ((y, t):env)
  | x == y    = Right t
  | otherwise = lookupType x env

typeCheck :: TypeEnv -> Expr -> Either String Type
typeCheck _ (EInt _) = Right TInt
typeCheck _ (EBool _) = Right TBool

typeCheck env (EVar x) =
  lookupType x env

typeCheck env (EIf cond eThen eElse) = do
  tCond <- typeCheck env cond
  if tCond /= TBool
    then Left "Condition in if must have type Bool"
    else do
      tThen <- typeCheck env eThen
      tElse <- typeCheck env eElse
      if tThen == tElse
        then Right tThen
        else Left "Both branches of if must have the same type"

typeCheck env (ELet x e1 e2) = do
  t1 <- typeCheck env e1
  typeCheck ((x, t1) : env) e2


typeCheck env (ELetRec f declaredType e1 e2) = do
  t1 <- typeCheck ((f, declaredType) : env) e1
  if t1 == declaredType
    then typeCheck ((f, declaredType) : env) e2
    else Left "letrec definition does not match declared type"

typeCheck env (EAdd e1 e2) = do
  t1 <- typeCheck env e1
  t2 <- typeCheck env e2
  if t1 == TInt && t2 == TInt
    then Right TInt
    else Left "Both arguments of + must have type Int"

typeCheck env (ESub e1 e2) = do
  t1 <- typeCheck env e1
  t2 <- typeCheck env e2
  if t1 == TInt && t2 == TInt
    then Right TInt
    else Left "Both arguments of - must have type Int"

typeCheck env (EMul e1 e2) = do
  t1 <- typeCheck env e1
  t2 <- typeCheck env e2
  if t1 == TInt && t2 == TInt
    then Right TInt
    else Left "Both arguments of * must have type Int"

typeCheck env (EEq e1 e2) = do
  t1 <- typeCheck env e1
  t2 <- typeCheck env e2
  if t1 == t2 && (t1 == TInt || t1 == TBool)
    then Right TBool
    else Left "Arguments of == must both have type Int or both have type Bool"

typeCheck env (ELt e1 e2) = do
  t1 <- typeCheck env e1
  t2 <- typeCheck env e2
  if t1 == TInt && t2 == TInt
    then Right TBool
    else Left "Both arguments of < must have type Int"

typeCheck env (ELam x tArg body) = do
  tBody <- typeCheck ((x, tArg) : env) body
  Right (TFun tArg tBody)

typeCheck env (EApp e1 e2) = do
  tFun <- typeCheck env e1
  tArg <- typeCheck env e2
  case tFun of
    TFun expectedArgType resultType ->
      if tArg == expectedArgType
        then Right resultType
        else Left "Function argument type mismatch"
    _ -> Left "Attempted to apply a non-function"

typeCheck _ (ENil t) =
  Right (TList t)

typeCheck env (ECons eHead eTail) = do
  tHead <- typeCheck env eHead
  tTail <- typeCheck env eTail
  case tTail of
    TList elemType ->
      if tHead == elemType
        then Right (TList elemType)
        else Left "Head of Cons must match list element type"
    _ -> Left "Second argument of Cons must have a list type"

typeCheck env (ECase e branches) = do
  tScrutinee <- typeCheck env e
  case tScrutinee of
    TList elemType ->
      typeCheckCase env elemType branches
    _ -> Left "Case expression currently only supports lists"

typeCheckCase :: TypeEnv -> Type -> [(Pattern, Expr)] -> Either String Type
typeCheckCase _ _ [] =
  Left "Case expression must have at least one branch"

typeCheckCase env elemType branches = do
  branchTypes <- mapM (checkBranch env elemType) branches
  case branchTypes of
    [] -> Left "Impossible: empty branches"
    (t:ts) ->
      if all (== t) ts
        then Right t
        else Left "All case branches must have the same type"

checkBranch :: TypeEnv -> Type -> (Pattern, Expr) -> Either String Type
checkBranch env elemType (pat, expr) =
  case pat of
    PNil ->
      typeCheck env expr

    PCons x xs ->
      typeCheck ((x, elemType) : (xs, TList elemType) : env) expr