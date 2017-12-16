data = "4	10	4	1	8	4	9	14	5	1	14	15	0	15	3	5"
l = map(int,data.strip().split())

cycles = 0
prevs = []

while l not in prevs:
    prevs.append(l[:])
    m = max(l)
    i = l.index(m)
    l[i] = 0
    while m:
        i=(i+1)%len(l)
        l[i]+=1
        m-=1
    cycles+=1