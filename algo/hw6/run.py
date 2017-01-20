
def calc_cost(p, D):
    cost,n,k = 0,len(D),len(p)
    for i in range(k-1):
        cost += D[p[i]][p[i+1]]
    if k == n:
        cost += D[p[-1]][p[0]]
    return cost

def tsp_brute_force(D):
    n, best_p = len(D), []
    best = n*max([max(e) for e in D])
    for p in permutations(range(n)):
        current = calc_cost(p, D)
        if current < best:
            best, best_p = current, p
    return best,best_p

def tsp_0(D):
    n,v = len(D),[]
    visit_0(v,n)

def visit_0(v,n):
    k = len(v)
    if k == n:
        print('Leaf node:     ', v)
    else:
        print('Internal node: ',v)
        vbar = complement(v,n)
        for e in vbar:
            visit_0(v+[e],n)

def complement(v,n):
    return [i for i in range(n) if i not in v]

def tsp_1(D):
    return visit_1([],D,sum(sum(e for e in d) for d in D))

def visit_1(v,D,best,best_p=[]):
    k,n = len(v),len(D)
    if k == n:
        cost = calc_cost(v,D)
        if cost < best:
            best,best_p = cost,v
    else:
        vbar = complement(v,n)
        for e in vbar:
            best,best_p = visit_1(v+[e],D,best,best_p)
    return best,best_p

def tsp_2(D):
    return visit_2([0],D,sum(sum(e for e in d) for d in D))
def visit_2(v,D,best,best_p=[]):
    return visit_1(v,D,best,best_p)

def tsp_3(D):
      return visit_3([0],D,sum(sum(e for e in d) for d in D))
def visit_3(v,D,best,best_p=[]):
    k,n = len(v),len(D)
    if k == n:
        cost = calc_cost(v,D)
        if cost < best:
            best,best_p = cost,v
    else:
        vbar = complement(v,n)
        for e in vbar:
            cost = calc_cost(v+[e],D)
            if cost < best:
                best,best_p = visit_3(v+[e],D,best,best_p)
    return best,best_p

def B(v,D):
    k,n = len(v),len(D)
    cost = calc_cost(v,D)
    W = complement(v,n)
    W0 = W + [v[0]]
    cost += min([D[v[k-1]][y] for y in W])
    cost += sum([min([D[y][z] for z in W0]) for y in W])
    return cost

def tsp_4(D):
    return visit_4([0],D,sum(sum(e for e in d) for d in D))
def visit_4(v,D,best,best_p=[]):
    k,n = len(v),len(D)
    if k == n:
        cost = calc_cost(v,D)
        if cost < best:
            best,best_p = cost,v
    else:
        vbar = complement(v,n)
        for e in vbar:
            cost = B(v,D)
            if cost < best:
                best,best_p = visit_4(v+[e],D,best,best_p)
    return best,best_p

from random import randint
from itertools import permutations
def test(n,i):
    D=[[randint(50,100) if k != j else 99999 for k in range(n)] for j in range(n)]
    if i == -1:
        return tsp_brute_force(D)
    elif i == 0:
        return tsp_0(D)
    elif i == 1:
        return tsp_1(D)
    elif i == 2:
        return tsp_2(D)
    elif i == 3:
        return tsp_3(D)
    elif i == 4:
        return tsp_4(D)
    elif i == 5:
        return tsp_5(D)
    elif i == 6:
        return tsp_6(D)
    elif i == 10:
        return tsp_dp(D)

print(test(6,0))
