module Lisp
  ( LispStruct(..)
  , eval
  )
where

import qualified Parser                        as P

data LispStruct
  = Atom String
  | -- list like types
    List [LispStruct]
  | DottedList [LispStruct] LispStruct
  | Lambda [LispStruct] LispStruct
  | -- special forms
    Define LispStruct LispStruct
  | IfExpr LispStruct LispStruct LispStruct
  | Quote LispStruct
  | -- primitive types
    Number Integer
  | String String
  | Boolean Bool
  deriving (Eq)

instance Show LispStruct where
  show (Atom    x) = x
  show (Number  i) = show i
  show (String  s) = "\"" ++ s ++ "\""
  show (Boolean x) = if x then "#t" else "#f"
  show (List    x) = "(" ++ (unwords . map show) x ++ ")"
  show (DottedList x y) =
    "(" ++ (unwords . map show) x ++ " . " ++ show y ++ ")"
  show (Define x y) = "(define " ++ show x ++ " " ++ show y ++ ")"
  show (Lambda x y) =
    "(lambda (" ++ (unwords . map show) x ++ ") " ++ show y ++ ")"
  show (IfExpr p r a) =
    "(if " ++ show p ++ " " ++ show r ++ " " ++ show a ++ ")"
  show (Quote q) = "(quote " ++ show q ++ ")"

eval :: P.LispVal -> LispStruct
eval (  P.Atom    a                          ) = Atom a
eval (  P.Number  n                          ) = Number n
eval (  P.String  s                          ) = String s
eval (  P.Boolean b                          ) = Boolean b
eval l@(P.List (P.Atom "define" : P.List (_ : _) : _)) = eval $ desugarFunc l
eval (  P.List    [P.Atom "define", var, val]) = Define (eval var) (eval val)
eval (P.List [P.Atom "lambda", P.List params, body]) =
  Lambda (map eval params) (eval body)
eval l@(P.List (P.Atom "lambda" : P.List _ : _)) = eval $ procLambda l
eval (P.List [P.Atom "if", pre, res, alt]) =
  IfExpr (eval pre) (eval res) (eval alt)
eval l@(P.List (P.Atom "let"  : _)      ) = eval $ desugarLet l      -- `let x = a in y` is just (\x -> y) a
eval c@(P.List (P.Atom "cond" : _)      ) = eval $ desugarCond c     -- `cond` is just chained `if`s
eval (  P.List [P.Atom "quote", content]) = Quote (eval content)
eval (  P.List xs                       ) = List (map eval xs)
eval (  P.DottedList x y                ) = DottedList (map eval x) (eval y)

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

desugarFunc :: P.LispVal -> P.LispVal
desugarFunc (P.List (P.Atom "define" : P.List (name : params) : contents)) =
  let lam = procLambda $ P.List (P.Atom "lambda" : P.List params : contents)
  in  P.List [P.Atom "define", name, lam]
desugarFunc x = x

-- transform function def in lambda into bindings
procLambda :: P.LispVal -> P.LispVal
procLambda l@(P.List [P.Atom "lambda", _, _]) = l
procLambda (P.List (P.Atom "lambda" : P.List params : body)) =
  let (ids, bds) = unzip . sep . map desugarFunc . init $ body
      content    = P.List [P.Atom "lambda", P.List params, last body]
  in  P.List $ [P.List [P.Atom "lambda", P.List ids, content]] ++ bds
 where
  sep []                            = []
  sep (P.List (_ : x : y : _) : xs) = (x, y) : sep xs
procLambda x = x
