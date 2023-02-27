# download package if not installed
if(!require("mosaicCalc")){
  install.packages("mosaicCalc")
}
library(mosaicCalc)

# Exercise 9.1
slice_plot(2*x+1 ~ x, domain(x = range(-5, 5)))

m = 2
b = 1
slice_plot(m*x+b ~ x, domain(x = range(-5, 5)))

slice_plot(a*x+b ~ x, domain(x = range(-5, 5)))

f = makeFun(2*x+1 ~ x)
slice_plot(f(x) ~ x, domain(x = range(-5, 5)))

f(x=2)

contour_plot(2*x + 3*y ~ x & y, domain(x=-5:5, y=-5:5))

interactive_plot(2*x + 3*y ~ x & y, domain(x=-5:5, y=-5:5))

# Exercise 9.2
f_prime = D(x^2+1 ~ x)
f_prime

f_prime(1)
f_prime(2)

f_prime = D(sin(x^2-5) ~ x)
f_prime

f_prime = D(2*x/(x+1) ~ x)
f_prime

f_prime = D(A*x^3+B*x+3 ~ x)
f_prime

f_prime(x=2, A=2, B=3)

slice_plot(f_prime(x, A=2, B=3) ~ x, domain(x=range(-5,5)))

# Exercise 9.3
f_pprime = D(A*x^3+B*x+3 ~ x & x)
f_pprime

f_pprime(x=2, A=2, B=3)
f_pprime(x=2, A=2, B=1)

slice_plot(f_pprime(x, A=2) ~ x, domain(x=range(-5,5)))

# Exercise 9.4
f_pprime = D(A*x^2 + B*x*y + C*y^2 ~ x & x)
f_pprime

f_pprime = D(A*x^2 + B*x*y + C*y^2 ~ x & y)
f_pprime

f_pprime = D(A*x^2 + B*x*y + C*y^2 ~ y & x)
f_pprime

f_pprime = D(A*x^2 + B*x*y + C*y^2 ~ y & y)
f_pprime

# Exercise 9.5
f = makeFun( A*x^2 + B*x + 3 ~ x)
f
f(1, A=1, B=1)

f_prime = D(f(x) ~ x)
f_prime
f_prime(x=1, A=1, B=1)

slice_plot(f(x, A=1, B=1) ~ x, domain(x = -1:1)) %>%
  gf_labs(title = "Original function f(x)")

slice_plot(f_prime(x, A=1, B=1) ~ x, domain(x =-1:1), color = "red") %>%
  gf_labs(title = "Derivative function f'(x)")

F_integral <- antiD(f_prime(x) ~ x)
F_integral

F_integral(1, A=1, B=1)

slice_plot(f_prime(x, A=1, B=1) ~ x, domain(x=-1:1), color = "red") %>%
  gf_labs(title = "Original function f'(x)")

slice_plot(F_integral(x, A=1, B=1) ~ x, domain(x=-1:1)) %>%
  gf_labs(title = "Antiderivative function F_integral(x)")

f_prime2 = D(F_integral(x) ~ x)
f_prime2

# Exercise 9.6
F_integral(x=1, A=1, B=1, C=0)
F_integral(x=1, A=1, B=1, C=1)
F_integral(x=1, A=1, B=1, C=2)

D(F_integral(x, A=1, B=1, C=0) ~ x)
D(F_integral(x, A=1, B=1, C=1) ~ x)
D(F_integral(x, A=1, B=1, C=2) ~ x)

f
f_prime
F_integral

F_integral(x=3, A=1, B=1) - F_integral(x=2, A=1, B=1)
