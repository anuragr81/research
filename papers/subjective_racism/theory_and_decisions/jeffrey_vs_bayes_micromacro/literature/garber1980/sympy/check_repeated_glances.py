import sympy as sp

p0, q0 = sp.Rational(3,10), sp.Rational(4,10)
alpha = sp.Rational(1,2)*sp.log((q0/p0)/((1-q0)/(1-p0)))
print("alpha =", sp.N(alpha, 6), " (Garber: .2209)")

def field_update(p, a):
    return (p*sp.exp(a)) / (p*sp.exp(a) + (1-p)*sp.exp(-a))

vals = [p0]
p = p0
for i in range(9):
    p = field_update(p, alpha)
    vals.append(p)

garber = [.3, .4, .5091, .6173, .7150, .7961, .8586, .9043, .9363, .9581]
print()
print("n : mine (4dp) : Garber's reported value")
for i, (v, g) in enumerate(zip(vals, garber)):
    print(f"{i} : {float(v):.4f} : {g}")

print()
print("=== second claim: p0=.3 -> q0=.5, five repetitions exceed .95 ===")
p0b, q0b = sp.Rational(3,10), sp.Rational(5,10)
alphab = sp.Rational(1,2)*sp.log((q0b/p0b)/((1-q0b)/(1-p0b)))
p = p0b
seq = [p0b]
for i in range(5):
    p = field_update(p, alphab)
    seq.append(p)
for i,v in enumerate(seq):
    print(f"P{i}(E) = {float(v):.4f}")
print("exceeds .95 after 5 reps:", float(seq[5]) > .95)
