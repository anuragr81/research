import csv, gzip, math, statistics, sys
from collections import defaultdict

def parse_octave(path):
    lines = open(path).read().splitlines()
    out, i = {}, 0
    while i < len(lines):
        if lines[i].startswith("# name:"):
            name = lines[i].split(":",1)[1].strip()
            typ = lines[i+1].split(":",1)[1].strip() if i+1 < len(lines) and lines[i+1].startswith("# type:") else ""
            if typ == "scalar":
                out[name] = float(lines[i+2].strip()); i += 3; continue
            if typ == "bool":
                out[name] = bool(int(lines[i+2].strip())); i += 3; continue
            if typ == "matrix":
                rows = int(lines[i+2].split(":")[1]); cols = int(lines[i+3].split(":")[1])
                vals = []
                j = i+4
                while len(vals) < rows*cols and j < len(lines):
                    s = lines[j].strip()
                    if s and not s.startswith("#"): vals.append(float(s))
                    j += 1
                out[name] = vals; i = j; continue
        i += 1
    return out

mat = parse_octave(sys.argv[1])
P = dict(r=mat['r'], mu=mat['mu'], mu_L=mat['mu_L'], sigma=mat['sigma'],
         sigma_L=mat['sigma_L'], c=mat['c'], a1=mat['a1'], a2=mat['a2'], a3=mat['a3'])
s_, S_ = mat['recap_edge_new'], mat['y_post_new']
y, pi_star, pi_max = mat['y'], mat['pi_star'], mat['pi_max']

print("=== MODEL SIDE (from the solve file) ===")
print(f"  pi_cap unset (0x0)            : {'pi_cap' not in mat or mat.get('pi_cap')==[]}")
print(f"  trigger s                     : {s_:.6f}   E/A {100*(1-1/s_):.2f}%")
print(f"  target  S                     : {S_:.6f}   E/A {100*(1-1/S_):.2f}%")
print(f"  gap S-s                       : {S_-s_:.6f}")
corner = sum(1 for a,b in zip(pi_star, pi_max) if abs(a-b) < 1e-9)
print(f"  pi* == pi_bar on              : {corner}/{len(y)} grid points   <- corner solution")

def model_sigma(x, pi=None):
    pib = max(0.0, min((1/P['a1'])*(1-1/x), (1/P['a3'])*(1-P['a2']/x))) if pi is None else pi
    s2 = (pib**2*P['sigma']**2*x**2 + 2*pib*P['c']*P['sigma']*P['sigma_L']*x*(1-x)
          + P['sigma_L']**2*(1-x)**2)
    return math.sqrt(max(0.0,s2)), pib

print()
print("=== DATA SIDE (from the panel) ===")
rows=[]
with gzip.open(sys.argv[2],'rt',newline='') as fh:
    rows=list(csv.DictReader(fh))
print(f"  bank-quarters                 : {len(rows):,}")

xs=[]; by=defaultdict(list); pi_obs=[]; cap_rwa=[]
for r in rows:
    try: A=float(r['ASSET']); L=float(r['LIAB'])
    except: continue
    if A>0 and L>0:
        x=A/L; xs.append(x); by[r['CERT']].append((r['REPDTE'],x))
    try:
        RWA=float(r['RWAJT']); T1=float(r['RBCT1J'])
        if A>0 and RWA>0: pi_obs.append(RWA/A); cap_rwa.append(T1/RWA)
    except: pass
xs.sort(); pi_obs.sort(); cap_rwa.sort()
q=lambda v,p: v[int(p*len(v))]
print(f"  x=A/L  p25/p50/p75            : {q(xs,.25):.4f} / {q(xs,.5):.4f} / {q(xs,.75):.4f}")
print(f"  E/A    p25/p50/p75            : {100*(1-1/q(xs,.25)):.2f}% / {100*(1-1/q(xs,.5)):.2f}% / {100*(1-1/q(xs,.75)):.2f}%")
print(f"  frac at/below model trigger   : {sum(1 for v in xs if v<=s_)/len(xs)*100:.2f}%")
print(f"  frac at/above model target    : {sum(1 for v in xs if v>=S_)/len(xs)*100:.2f}%")

sds=[];lv=[]
for k,v in by.items():
    v.sort(); seq=[x for _,x in v]
    if len(seq)<8: continue
    d=[seq[i+1]-seq[i] for i in range(len(seq)-1)]
    try: sd=statistics.stdev(d)
    except: continue
    if sd>0: sds.append(sd); lv.append(statistics.median(seq))
qsd=statistics.median(sds); lvl=statistics.median(lv)
ann=qsd*2
print(f"  realised sd(dx) quarterly     : {qsd:.5f}  ({len(sds):,} banks >=8q)")
print(f"  realised annualised           : {ann:.5f}  at x={lvl:.5f}")

print()
print("=== CORNER SOLUTION TEST (regulatory basis) ===")
print(f"  measured pi = RWA/ASSET  p50  : {q(pi_obs,.5):.4f}")
print(f"  model pi_bar at x={lvl:.4f}     : {model_sigma(lvl)[1]:.4f}")
print(f"  measured capital/RWA     p50  : {q(cap_rwa,.5)*100:.2f}%   = {q(cap_rwa,.5)/P['a1']:.2f}x a1={P['a1']}")
print(f"                           p25  : {q(cap_rwa,.25)*100:.2f}%   = {q(cap_rwa,.25)/P['a1']:.2f}x a1")

print()
print("=== SIGMA LEVEL ===")
sc,_ = model_sigma(lvl)
so,_ = model_sigma(lvl, pi=q(pi_obs,.5))
print(f"  model sigma at pi*=pi_bar     : {sc:.5f}   = {sc/ann:.1f}x measured")
print(f"  model sigma at OBSERVED pi    : {so:.5f}   = {so/ann:.1f}x measured")
