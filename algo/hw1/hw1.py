import random
import time
from sys import maxint

# Given an array , find two elements ,
# if there are such two, that sum to zero.
def zero_sum(a):
    for i in range(len(a)-1):
        for j in range(i+1,len(a)):
            if a[i]+a[j] == 0:
                return i , j
    return -1, -1

def zero_sum_s ( a ) :
    a = sorted(a)
    l,r = 0, len(a)-1
    while l < r:
        s = a[l]+a[r]
        if s==0:
            return a[l],a[r]
        elif s<0:
            l = l+1
        elif s>0:
            r = r-1
    return None,None

def zero_sum_smartest(a):
    s = {}
    for i,e in enumerate(a):
        if -e in s:
            return i, s[-e]
        else:
            s[e] = i
    return None

def rand_pos_array(n):
    return [random.randint(0, maxint) for i in range(n)]

def timeit(func, arr, iterations=20):
    # run for a number of iterations and return the average runtime
    # in milliseconds
    l = []
    for i in range(iterations):
        start = time.time()
        func(arr)
        end = time.time()
        l.append(1000* (end-start))
    return reduce(lambda x, y: x+y, l) / float(len(l))

def main():
    i, b, o, s = [],[],[],[]
    for n in range(10, 500, 10):
        print(n)
        arr = rand_pos_array(n)
        i.append(str(n))
        b.append(timeit(zero_sum, arr))
        o.append(timeit(zero_sum_s, arr))
        s.append(timeit(zero_sum_smartest, arr))
        bs = map("{:.3f}".format, b)
        os = map("{:.3f}".format, o)
        ss = map("{:.3f}".format, s)
    print "," + ",".join(i)
    print "brute," + ",".join(bs)
    print "smarter," + ",".join(os)
    print "smartest," + ",".join(ss)

if __name__ == "__main__":
    main()
