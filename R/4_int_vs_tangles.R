library(Momocs)
library(pracma)
library(tidyverse)

# ---------------------------------------------
# tangles vs integration



# ----------------------
# tangles on unit circle

x2 <- seq(0, sqrt(3)/2, by = 0.001)
x2 <- x_param # from "1_dev_tangles.R"
y2 <- sqrt(1-(x2^2))

coords <- matrix(c(x2, y2), ncol =2)


tangvect <- coords[-1,] - coords[-nrow(coords),]

tet1 <-
  Arg(complex(
    real = tangvect[, 1],
    imaginary = tangvect[, 2]))


phi <-
  abs(tet1[-1] - tet1[-length(tet1)])


# arclength
circlefun <- function(t) c(t, sqrt(1-(t^2)))
arc <- arclength(circlefun, 0, sqrt(3)/2)$length

# for some reason, sum(phi) doesn't need to be adjusted,
# it just works.
sum(phi)



# --------------------------
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

# agrees with tangles
atan2(int, f0(int))


# ----------------------
# tangles on polynomial

x5 <- seq(1, 10, by = 0.001)
y5 <- x5^2

coords <- matrix(c(x5, y5), ncol =2)


tangvect <- coords[-1,] - coords[-nrow(coords),]

tet1 <-
  Arg(complex(
    real = tangvect[, 1],
    imaginary = tangvect[, 2]))


phi <-
  abs(tet1[-1] - tet1[-length(tet1)])


# K_tot =  0.4134868 for y=x^2 for x=[1, 10]
sum(phi)


# --------------------------
# integration on polynomial

f0 <- function(x) x^2

f3 <- function(x) {

  f1 <- Deriv::Deriv(f0)
  f2 <- Deriv::Deriv(f1)
  ((f2(x)) / ((1 + (f1(x)^2))^1.5)) *
    (sqrt(1+(f1(x))^2))
}

# integrate() spits out the arclength
# so, how convert arclength to curvature (radians)?
int <- integrate(f3, lower = 1, upper = 10)$value

# does not agree with tangles
atan2(int, f0(int))
