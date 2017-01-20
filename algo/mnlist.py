import random

def fisheryates(inp):
    l = len(inp)
    for i in range(l):
        j = random.randint(i,l-1)
        inp[i], inp[j] = inp[j], inp[i]

    return inp

def main(m,n):
    a = list(range(1,n+1))
    b = fisheryates(a)
    print(sorted(a[:m]))

if __name__ == "__main__":
    m = 3
    n = 8
    main(m,n)
