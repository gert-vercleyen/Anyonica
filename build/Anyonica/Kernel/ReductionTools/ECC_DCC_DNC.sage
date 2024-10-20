UCF=UniversalCyclotomicField()

def Checking(M):
    r=len(M)
    N=zero_matrix(QQ,r)
    for i in range(r):
        Mi=matrix(QQ,M[i])
        for j in range(i):
        Mj=matrix(QQ,M[j])
        if Mi*Mj!=Mj*Mi:
            return ’non-commutative’
        Ti=Mi.transpose()
        Ni=Mi*Ti
        N+=Ni
    f = N.minpoly()
    ff=N.charpoly()
    if not Cyclo(M):
        return ’non-cyclo’ # Extended cyclotomic criterion
    K.<a> = f.splitting_field()
    n = K.conductor()
    L=ff.roots(CyclotomicField(n))
    LL=[UCF(l[0]) for l in L]
    rL=[l[0].n() for l in L]
    mm=max(rL)
    for ii in range(len(L)):
        if mm==rL[ii]:
            dim=LL[ii]
            break
    c=0
    for x in LL:
        d=0
        for y in LL:
            yy=UCF(x/y)
            if ’/’ in list(str(yy)):
            d=1
            break
        if d==0:
        c=1
    if c==0:
        print(’non-Drinfeld’) # Drinfeld center criterion
    for x in LL:
        p=list(UCF(x).minpoly())
        n=len(p)-1
        A=p[0]
        d=0
        for i in range(n+1):
            a=p[i]
            j=n-i
            y=UCF((aˆn)/(Aˆj))
            if ’/’ in list(str(y)):
                d=1
                break
        if d==1:
            print(’non-d-number’) # d-number criterion
            break

def Cyclo(M):
    r=len(M)
    for k in range(len(M)):
        N=matrix(QQ,M[k])
        f = N.minpoly()
        K.<a> = f.splitting_field()
        if not K.is_abelian():
            return false
    return true