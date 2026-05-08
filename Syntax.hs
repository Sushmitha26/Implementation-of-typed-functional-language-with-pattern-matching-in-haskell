module Syntax where

type Name = String

data Type
  = TInt
  | TBool
  | TFun Type Type
  | TList Type
  deriving (Eq, Show)

data Pattern
  = PNil
  | PCons Name Name
  deriving (Eq, Show)

data Expr
  = EInt Int
  | EBool Bool
  | EVar Name
  | EIf Expr Expr Expr
  | ELet Name Expr Expr
  | ELetRec Name Type Expr Expr
  | ELam Name Type Expr
  | EApp Expr Expr
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EEq Expr Expr
  | ELt Expr Expr
  | ENil Type
  | ECons Expr Expr
  | ECase Expr [(Pattern, Expr)]
  deriving (Eq, Show)

data Value
  = VInt Int
  | VBool Bool
  | VClosure Name Expr Env
  | VNil
  | VCons Value Value
  deriving (Eq, Show)

type Env = [(Name, Value)]
type TypeEnv = [(Name, Type)]