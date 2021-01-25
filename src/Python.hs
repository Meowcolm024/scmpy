{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}

module Python where

import           Text.InterpolatedString.Perl6  ( q
                                                , qq
                                                )

{-
Library for translating to Python
might be changed to using `qq` instead of `q` later
-}

-- Standard library for scmpy
scmpy :: String
scmpy = [q|#### -- scmpy lib starts -- ####
def _list(*kwargs): 
    return list(kwargs)

def cons(x, xs):
    if type(xs) == list:
        return [x] + xs
    if type(xs) == tuple:
        return (x, xs)

def car(xs): 
    return xs[0]

def cdr(xs):
    if type(xs) == list:
        return xs[1:]
    if type(xs) == tuple:
        return xs[2]

def _if(pred, result, alt): 
    return result if pred else alt
#### --  scmpy lib end  -- ####
|]

-- * example of converting (temp)

-- * later will be something like :: LispVal -> String

define :: String -> String -> String
define var content = [qq|{var} = {content}|]
