#### -- scmpy lib starts -- ####
import operator
import functools

def display(*kwargs):
    for i in kwargs:
        print(i, end=' ')

_list = lambda *kwargs: list(kwargs)
displayln = print
cons = lambda x, xs:[x] + xs if type(xs) == list else (x, xs) if type(xs) == tuple else None
car = lambda xs: xs[0]
cdr = lambda xs: xs[1:] if type(xs) == list else xs[1] if type(xs) == tuple else None
_null = lambda x: len(x) == 0
null = ()
_if = lambda pred, result, alt: result if pred else alt
_add = lambda *kwargs: functools.reduce(operator.add, list(kwargs))
_sub = lambda *kwargs: functools.reduce(operator.sub, list(kwargs))
_mul = lambda *kwargs: functools.reduce(operator.mul, list(kwargs))
_div = lambda *kwargs: functools.reduce(operator.truediv, list(kwargs))
_eq = lambda *kwargs: functools.reduce(operator.eq, list(kwargs))
_and = lambda *kwargs: functools.reduce(operator.and_, list(kwargs))
_or = lambda *kwargs: functools.reduce(operator.or_, list(kwargs))
_not = lambda x: not x
_listq = lambda x: type(x) == list or type(x) == tuple

#### --  scmpy lib end  -- ####
