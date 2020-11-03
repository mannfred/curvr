library(Momocs)
library(pracma)
library(tidyverse)

# ----------------------------------------------

f0 <- function(x) sqrt(1-(x^2))

f3 <- function(x) {

  f1 <- Deriv::Deriv(function(x) sqrt(1-(x^2)))
  f2 <- Deriv::Deriv(f1)
  abs(f2(x)) / ((1 + (f1(x)^2))^1.5)
}

xybounds <- c(sqrt(3)/2, f0(sqrt(3)/2))


# integrate() spits out the arclength
# so, how convert arclength to curvature (radians)?
integrate(f3, lower = 0, upper = sqrt(3)/2)

# agrees with tangles
atan2(0.8660254, 0.5)




# ---------------------------------------------
x2 <- seq(0, sqrt(3)/2, by = 0.001)
y2 <- sqrt(1-(x2^2))

coords <- matrix(c(x2, y2), ncol =2)


tangvect <- coords[-1,] - coords[-nrow(coords),]

tet1 <-
  Arg(complex(
    real = tangvect[, 1],
    imaginary = tangvect[, 2]))


phi <-
  abs(tet1[-1] - tet1[-length(tet1)])


# K_tot = 0.7634432 (approx pi/4) for unit circle
sum(phi)





# ------------------------------------
# integration vs summation (they can agree!)

# summation ----
x <- seq(1, 10, by=0.00001)
dfun <- deriv3(expression(x^2), "x", func=TRUE)
gr <- attr(dfun(x), "gradient") #computes 1st derivative bw x=0 to x=1
he <- attr(dfun(x), "hessian")[ , , "x"] #computes 2nd derivative bw x=0 to x=1
k <- abs(he)/ ((1 + gr^2)^1.5)
adj <- (length(x) - 1) / (max(x)-min(x))
sum(k) / adj
# 0.104326


# integration ----
f5 <- function(x) {

  f1 <- Deriv::Deriv(function(x) x^2)
  f2 <- Deriv::Deriv(f1)
  abs(f2(x)) / ((1 + (f1(x)^2))^1.5)
}


# 0.1043251 with absolute error < 3.6e-05
# but this is parameterized by arclength!
# agrees with Wolfram: https://www.wolframalpha.com/input/?i=integrate+2+%2F+%281%2B%282x%29%5E2%29%29%29%5E1.5+from+1+to+10
ktot <- integrate(f5, lower = 1, upper = 10)[[1]]




# integration vs tangles ------------------


# arclength ----

# 99.56836
f6 <- function(t) c(t,t^2)
al <- arclength(f6, 1, 10)$length



