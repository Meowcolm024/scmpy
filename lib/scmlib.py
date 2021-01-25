#### -- scmpy lib starts -- ####
import operator
import functools

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

def add(*kwargs):
    return functools.reduce(operator.add, list(kwargs))

def sub(*kwargs):
    return functools.reduce(operator.sub, list(kwargs))

def mul(*kwargs):
    return functools.reduce(operator.mul, list(kwargs))

def div(*kwargs):
    return functools.reduce(operator.truediv, list(kwargs))

def eq(*kwargs):
    return functools.reduce(operator.eq, list(kwargs))

def _and(*kwargs):
    return functools.reduce(operator.and_, list(kwargs))

def _or(*kwargs):
    return functools.reduce(operator.or_, list(kwargs))

def _not(x):
    return not x
#### --  scmpy lib end  -- ####
