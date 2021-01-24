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
    if pred:
        return result
    else:
        return alt
