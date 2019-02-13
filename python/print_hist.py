def print_hist(h):
    for c in sorted(h):
        print(c, h[c])

def histogram(s):
    d = dict()
    for c in s:
        d[c] = d.get(c,0) + 1
    return d

h = histogram('parrot')

print_hist(h)

