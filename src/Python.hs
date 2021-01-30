{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}

module Python where

import           Text.InterpolatedString.Perl6  ( q
                                                , qq
                                                )
import           Data.List                      ( intersperse )

{-
Library for translating to Python
might be changed to using `qq` instead of `q` later
-}

-- Standard library for scmpy
scmlib :: String
scmlib = [q|#### -- scmpy lib starts -- ####
import operator
import functools

def chained(op, xs):
    if len(xs) <= 1:
        return False
    x = op(xs[0], xs[1])
    for i in range(1, len(xs) - 2):
        x = x and op(xs[i], xs[i+1])
    return x

def display(*kwargs):
    for i in kwargs:
        print(i, end=' ')

list_ = lambda *kwargs: list(kwargs)
displayln = print
cons = lambda x, xs:[x] + xs if type(xs) == list else (x, xs) if type(xs) == tuple else None
car = lambda xs: xs[0]
cdr = lambda xs: xs[1:] if type(xs) == list else xs[1] if type(xs) == tuple else None
null_ = lambda x: len(x) == 0
null = ()
add_ = lambda *kwargs: functools.reduce(operator.add, list(kwargs))
sub_ = lambda *kwargs: functools.reduce(operator.sub, list(kwargs))
mul_ = lambda *kwargs: functools.reduce(operator.mul, list(kwargs))
div_ = lambda *kwargs: functools.reduce(operator.truediv, list(kwargs))
eq_ = lambda *kwargs: functools.reduce(operator.eq, list(kwargs))
and_ = lambda *kwargs: functools.reduce(operator.and_, list(kwargs))
or_ = lambda *kwargs: functools.reduce(operator.or_, list(kwargs))
not_ = lambda x: not x
listq_ = lambda x: type(x) == list or type(x) == tuple
lt_ = lambda *kwargs: chained(operator.lt, list(kwargs))
le_ = lambda *kwargs: chained(operator.le, list(kwargs))
gt_ = lambda *kwargs: chained(operator.gt, list(kwargs))
ge_ = lambda *kwargs: chained(operator.ge, list(kwargs))
map_ = lambda func, *iterable: list(map(func, *iterable))
filter_ = lambda func, *iterable: list(filter(func, *iterable))
#### --  scmpy lib end  -- ####

|]

reformat :: [String] -> String
reformat = concat . intersperse ","

define :: String -> String -> String
define var content = [qq|{var} = {content}|]

funcApp :: String -> [String] -> String
funcApp fn args = let arg' = reformat args in [qq|{fn}({arg'})|]

list :: [String] -> String
list l = let l' = reformat l in [qq|[{l'}]|]

pair :: String -> String -> String
pair x y = [qq|({x},{y})|]

lambda :: [String] -> String -> String
lambda args body = let arg' = reformat args in [qq|(lambda {arg'}: {body})|]
