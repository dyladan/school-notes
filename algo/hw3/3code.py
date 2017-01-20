import itertools

lst = [1,2,3,4, 5,6,7,8,9,10]
k = 3

def subsets(lst, k):
    n = len(lst)
    perms = [i for i in itertools.combinations(range(n), k)]
    print perms

subsets(lst, k)

