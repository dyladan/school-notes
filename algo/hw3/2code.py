import operator

def max_kprod(lst, r):
    pool = tuple(lst)
    n = len(pool)
    if r > n:
        return
    idxs = list(range(r))
    ret = tuple(pool[i] for i in idxs)
    max_prod = reduce(operator.mul, ret, 1)
    while True:
        for i in reversed(range(r)):
            if idxs[i] != i + n - r:
                break
        else:
            return ret
        idxs[i] += 1
        for j in range(i+1, r):
            idxs[j] = idxs[j-1] + 1
        t = tuple(pool[i] for i in idxs)
        prod = reduce(operator.mul, t, 1)
        if prod > max_prod:
            max_prod = prod
            ret = t

print max_kprod([1,2,3,5], 2)
print max_kprod([1,-2,3,5], 2)
print max_kprod([1,2,-3,5], 2)
print max_kprod([1,2,3,5], 2)
print max_kprod([1,-2,3,-5], 2)
print max_kprod([-1,2,-3,5], 2)
