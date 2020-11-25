library(Momocs)
library(pracma)
library(tidyverse)

# ----------------------------------------------
# integration on unit circle

f0 <- function(x) sqrt(1-(x^2))

f3 <- function(x) {

  f1 <- Deriv::Deriv(function(x) sqrt(1-(x^2)))
  f2 <- Deriv::Deriv(f1)
  abs(f2(x)) / ((1 + (f1(x)^2))^1.5)
}

# integrate() spits out the arclength
# so, how convert arclength to curvature (radians)?
int <- integrate(f3, lower = 0, upper = sqrt(2)/2)$value



# ----------------------------------------------
# integration on polynomial
f5 <- function(x) {

  f1 <- Deriv::Deriv(function(x) x^2)
  f2 <- Deriv::Deriv(f1)
  abs(f2(x)) / ((1 + (f1(x)^2))^1.5)
}


# 0.1043251 with absolute error < 3.6e-05
# but this is parameterized by arclength!
# agrees with two Wolfram:
# https://www.wolframalpha.com/input/?i=integrate+2+%2F+%281%2B%282x%29%5E2%29%29%29%5E1.5+from+1+to+10
ktot <- integrate(f5, lower = 0, upper = sqrt(3)/2)$value

# arclength
circlefun <- function(t) c(t, sqrt(1-(t^2)))
al <- arclength(circlefun, 0, sqrt(3)/2)$length

# K_tot
# need to change this since we're actually integrating
# and not approx. integral with a sum
(al/(length(x)-1)) * sum(k)



# ------------------------------------
# summation on unit circle

x <- seq(0, sqrt(3)/2, by=0.001)
dfun <- deriv3(expression(sqrt(1-(x^2))), "x", func=TRUE)
gr <- attr(dfun(x), "gradient") #computes 1st derivative bw x=0 to x=1
he <- attr(dfun(x), "hessian")[ , , "x"] #computes 2nd derivative bw x=0 to x=1
k <- abs(he)/ ((1 + gr^2)^1.5)
# agrees with Wolfram
# # https://www.wolframalpha.com/widgets/view.jsp?id=477ce31923bd5cc8f9d1d0502062c445

# arclength
circlefun <- function(t) c(t, sqrt(1-(t^2)))
al <- arclength(circlefun, 0, sqrt(3)/2)$length

# K_tot
# for every piece (i) of arclength (al/length), multiply
# that piece with its corresponding curvature (k_i)
(al/(length(x)-1)) * sum(k)



# ------------------------------------
# summation on polynomial

x <- seq(1, 10, by=0.001)
dfun <- deriv3(expression(x^2), "x", func=TRUE)
gr <- attr(dfun(x), "gradient") #computes 1st derivative bw x=0 to x=1
he <- attr(dfun(x), "hessian")[ , , "x"] #computes 2nd derivative bw x=0 to x=1
k <- abs(he)/ ((1 + gr^2)^1.5)
# agrees with Wolfram
# # https://www.wolframalpha.com/widgets/view.jsp?id=477ce31923bd5cc8f9d1d0502062c445

# arclength
# circlefun <- function(t) c(t, sqrt(1-(t^2)))
polyfun <- function(t) c(t, t^2)
al <- arclength(polyfun, 1, 10)$length

# K_tot
# for every piece (i) of arclength (al/length), multiply
# that piece with its corresponding curvature (k_i)
(al/(length(x)-1)) * sum(k)











