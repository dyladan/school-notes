def read_dat(fname):
    with open(fname) as f:
        for l in f.readlines():
            yield([int(s.strip()) for s in l.split(',')])

def max_3prod(a):
    n = len(a)
    if n < 3:
        return
    ml = a[:3]
    m = a[0] * a[1] * a[2]
    for i in range(n-2):
        for j in range(i+1, n-1):
            for k in range(j+1, n):
                t = m
                m = max(m, a[i] * a[j] * a[k])
                if m > t:
                    ml = [a[i], a[j], a[k]]
    return ml, m


s = 0
for i in read_dat('sum-of-k.data'):
    lst, p = max_3prod(i)
    s += p
    print '[' + " ".join(str(i) for i in lst) + ']  ->', p

print "TOTAL:", s
