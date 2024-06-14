# arguments
r2 = 0.1
f2 = r2 / (1 - r2)
u = 5
power = 0.90
sig.level = 0.05

# Calculate the sample size
result = pwr.f2.test(u = u, f2 = f2, sig.level = sig.level, power = power)

# Calculate total sample size
N = result$v + u + 1
N
