import csv, gzip, statistics, sys
from collections import defaultdict

T = 0.005
X_MAX = 1.39

rows=[]
with gzip.open(sys.argv[1],'rt',newline='') as fh:
    rows=list(csv.DictReader(fh))

by=defaultdict(dict)
for r in rows:
    try:
        cert=r['CERT']; d=r['REPDTE']
        A=float(r['ASSET']); L=float(r['LIAB'])
    except: continue
    if A<=0 or L<=0: continue
    try: ytd=float(r['EQCSTKRX'])
    except: ytd=None
    by[cert][d]=dict(A=A, x=A/L, ytd=ytd)

QS=['0331','0630','0930','1231']
episodes=defaultdict(list)
for cert, series in by.items():
    dates=sorted(series)
    for i,d in enumerate(dates):
        rec=series[d]
        if rec['ytd'] is None: continue
        yr, mmdd = d[:4], d[4:]
        if mmdd==QS[0]:
            flow=rec['ytd']
        else:
            pidx=QS.index(mmdd)-1
            prev=yr+QS[pidx]
            if prev not in series or series[prev]['ytd'] is None: continue
            flow=rec['ytd']-series[prev]['ytd']
        if flow<=0: continue
        f=flow/rec['A']
        if f < T: continue
        if i==0: continue
        pre_d=dates[i-1]
        pre=series[pre_d]['x']; post=rec['x']
        if pre>=X_MAX or post>=X_MAX: continue
        episodes[cert].append((pre,post,f))

allep=[e for v in episodes.values() for e in v]
print(f"episodes (all banks)   : {len(allep)}  across {len(episodes)} banks")

rep={k:v for k,v in episodes.items() if len(v)>=2}
n=sum(len(v) for v in rep.values())
print(f"within-bank sample     : {n} episodes across {len(rep)} banks")

dpre=[];dpost=[];df=[]
for k,v in rep.items():
    mp=statistics.fmean([e[0] for e in v])
    mq=statistics.fmean([e[1] for e in v])
    mf=statistics.fmean([e[2] for e in v])
    for pre,post,f in v:
        dpre.append(pre-mp); dpost.append(post-mq); df.append(f-mf)

def slope_se(yv,xv):
    sxx=sum(a*a for a in xv)
    b=sum(a*c for a,c in zip(xv,yv))/sxx
    res=[c-b*a for a,c in zip(xv,yv)]
    dof=len(xv)-1
    s2=sum(r*r for r in res)/dof
    return b, (s2/sxx)**0.5

b_pre,se_pre = slope_se(dpre,df)
b_post,se_post = slope_se(dpost,df)
xbar=statistics.fmean([e[0] for v in rep.values() for e in v])

print()
print(f"x_bar                  : {xbar:.4f}")
print(f"b_pre                  : {b_pre:+.4f}  (se {se_pre:.4f})")
print(f"b_post                 : {b_post:+.4f}  (se {se_post:.4f})")
print(f"identity residual      : {(b_post-b_pre)-xbar:+.4f}")
print()
print(f"t vs mechanical  (b_pre=0)     : {abs(b_pre/se_pre):.2f}")
print(f"t vs exact-target(b_pre=-xbar) : {abs((-xbar-b_pre)/se_pre):.2f}")
