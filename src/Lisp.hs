module Lisp where

import qualified Parser                        as P

data LispStruct
  = Atom String
  | -- list like types
    List [LispStruct]
  | DottedList [LispStruct] LispStruct
  | Lambda [LispStruct] LispStruct      -- ! should be: Lambda [LispStruct] [LispStruct]
  | -- special forms
    Define LispStruct LispStruct
  | IfExpr LispStruct LispStruct LispStruct
  | Quote LispStruct
  | -- primitive types
    Number Integer
  | String String
  | Boolean Bool
  deriving (Show, Eq)

eval :: P.LispVal -> LispStruct
eval (P.Atom    a) = Atom a
eval (P.Number  n) = Number n
eval (P.String  s) = String s
eval (P.Boolean b) = Boolean b
eval (P.List [P.Atom "define", P.List (name : params), contents]) =
  -- desugar function definition
  eval $ P.List
    [P.Atom "define", name, P.List [P.Atom "lambda", P.List params, contents]]
eval (P.List [P.Atom "define", var, val]) = Define (eval var) (eval val)
eval (P.List [P.Atom "lambda", P.List params, body]) =
  Lambda (map eval params) (eval body)
eval (P.List [P.Atom "if", pre, res, alt]) =
  IfExpr (eval pre) (eval res) (eval alt)
eval l@(P.List (P.Atom "let" : _)) = eval $ desugarLet l
eval c@(P.List (P.Atom "cond" : _)) = eval $ desugarCond c
eval (P.List [P.Atom "quote", content]) = Quote (eval content)
eval (P.List xs) = List (map eval xs)
eval _ = undefined

desugarLet :: P.LispVal -> P.LispVal
desugarLet (P.List [P.Atom "let", P.List bindings, content]) =
  let (ids, bds) = unzip $ sep bindings
  in  P.List $ [P.List [P.Atom "lambda", P.List ids, content]] ++ bds
 where
  sep []                        = []
  sep (P.List (x : y : _) : xs) = (x, y) : sep xs
desugarLet x = x

desugarCond :: P.LispVal -> P.LispVal
desugarCond (P.List (P.Atom "cond" : conds)) = desugar conds
 where
  desugar []                            = P.Atom "None"
  desugar [P.List [P.Atom "else", alt]] = alt
  desugar (P.List [cond, result] : rest) =
    P.List $ [P.Atom "if", cond, result, desugar rest]
desugarCond x = x
