def print_hist(h):
    for c in sorted(h):
        print(c, h[c])

def histogram(s):
    d = dict()
    for c in s:
        d[c] = d.get(c,0) + 1
    return d

def reverse_lookup(d,v):
    for k in d:
        if d[k] == v:
            return k
    raise LookupError('value does not appear in the dictionary')

def invert_dict(d):
    inverse = dict()
    for key in d:
        val = d[key]
        if val not in inverse:
            inverse[val] = [key]
        else:
            inverse[val].append(key)
    return inverse

h = histogram('parrot')

print_hist(h)

key = reverse_lookup(h, 2)

print(key)

inverse = invert_dict(h)

print_hist(inverse)
