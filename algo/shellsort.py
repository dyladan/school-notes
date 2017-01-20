gaps = [23, 10, 5, 1]

def shellSort(lst):
    for gap in gaps:
        for i in range(gap):
            gapISort(lst, i, gap)
    return lst

def gapISort(lst,start,gap):
    for i in range(start+gap, len(lst), gap):
        v = lst[i]
        p = i
        while p >= gap and lst[p-gap] > v:
            lst[p] = lst[p-gap]
            p = p - gap
        lst[p] = v

print shellSort([42,12,43,64,13,354,64,13,34,646,4523,31])
