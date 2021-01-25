module Lisp where

import qualified Parser as P

data LispStruct
  = -- list like types
    Atom String
  | List [LispStruct]
  | DottedList [LispStruct] LispStruct
  | Lambda [LispStruct] LispStruct
  | Function LispStruct [LispStruct] LispStruct
  | -- special forms
    Define LispStruct LispStruct
  | LetExpr [(LispStruct, LispStruct)] LispStruct
  | IfExpr LispStruct LispStruct LispStruct
  | CondEXpr [(LispStruct, LispStruct)]
  | Quote LispStruct
  | -- primitive types
    Number Integer
  | String String
  | Boolean Bool
  deriving (Show, Eq)

eval :: P.LispVal -> LispStruct
eval (P.Atom a) = Atom a
eval (P.Number n) = Number n
eval (P.String s) = String s
eval (P.Boolean b) = Boolean b
eval (P.List [P.Atom "define", var, val]) = Define (eval var) (eval val)
eval (P.List [P.Atom "lambda", P.List params, body]) = Lambda (map eval params) (eval body)
eval (P.List [P.Atom "if", pre, res, alt]) = IfExpr (eval pre) (eval res) (eval alt)
eval _ = undefined
