#### -- scmpy lib starts -- ####
import operator
import functools

def _list(*kwargs):
    return list(kwargs)

def display(*kwargs):
    for i in kwargs:
        print(i, end=' ')

displayln = print

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

def _null(x):
    return len(x) == 0

null = ()

def _if(pred, result, alt):
    return result if pred else alt

def _add(*kwargs):
    return functools.reduce(operator.add, list(kwargs))

def _sub(*kwargs):
    return functools.reduce(operator.sub, list(kwargs))

def _mul(*kwargs):
    return functools.reduce(operator.mul, list(kwargs))

def _div(*kwargs):
    return functools.reduce(operator.truediv, list(kwargs))

def _eq(*kwargs):
    return functools.reduce(operator.eq, list(kwargs))

def _and(*kwargs):
    return functools.reduce(operator.and_, list(kwargs))

def _or(*kwargs):
    return functools.reduce(operator.or_, list(kwargs))

def _not(x):
    return not x
#### --  scmpy lib end  -- ####
