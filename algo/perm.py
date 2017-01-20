def permutations(lst, chosen=[]):
    if len(lst) == 0:
        return chosen
    for i in range(len(lst)):
        return permutations(lst[:i] + lst[i+1:], chosen.append(lst[i]))

import pdb
pdb.set_trace()
print(permutations([1,2,3], []))
