module Lisp where

import qualified Parser as P

data LispStruct
  = Atom String
  | List [LispStruct]
  | DottedList [LispStruct] LispStruct
  | Lambda [String] [LispStruct]
  | Function String [String] [LispStruct]
  | Number Integer
  | String String
  | Boolean Bool
  deriving (Show, Eq)

generate :: P.LispVal -> LispStruct
generate = undefined
