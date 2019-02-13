"""
def min_max(t):
    return min(t), max(t)

result = (min_max((10,5,9,1023,24,54)))

def printall(*args):
    print(args)

printall(result)

def has_match(t1,t2):
    for x,y in zip(t1,t2):
        if x == y:
            return True
        return False

for index, element in enumerate('abc'):
    print(index, element)
"""

def most_frequent(string):
    co = dict()
    for s in string:
        co[s]=co.setdefault(s,0)+1
    return co

def invert_dict(di):
    co = dict()
    for v,k in list(zip(di.values(),di.keys())):
        co[v]=co.get(v,list())
        co[v].append(k)
    return co

print(invert_dict(most_frequent('pprivet')))
