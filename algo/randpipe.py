import random

def getrand(l):
	n = 0
	c = None
	for e in l:
		n += 1
		r = random.randint(1, n)
		if r == 1:
			c = e
	return c

print(getrand(range(11)))