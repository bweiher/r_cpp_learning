from numpy import size as npsize


def pyway(x):
    total = 0
    n = npsize(x)
    for i in range(n):
        total = total + x[i]
    return(total)
