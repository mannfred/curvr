library(Momocs)
library(pracma)
library(tidyverse)

# --------------------------------------
# set up mock LM data
x <- 1:10
y <- x^2
mdat <- matrix(c(x,y), nrow=10, ncol=2)
my_poly <- Momocs::npoly(mdat, 2)

# point-wise K between 1 and 10
k <-
  total_curvature(my_poly, c(1, 10), 1000)

# convert arc length to radians
alrad <- k$s * (pi/180)

alrad * k$total_k
# 0.4227657


# all K values from curvr check out with
# https://www.wolframalpha.com/input/?i=curvature+of+y%3Dx%5E2+at+x%3D10
# but what units does Wolfram use?

# testing against Wolfram
# https://www.wolframalpha.com/widgets/view.jsp?id=477ce31923bd5cc8f9d1d0502062c445
x <- seq(1, 10, by=0.001)
dfun <- deriv3(expression(x^2), "x", func=TRUE)
gr <- attr(dfun(x), "gradient") #computes 1st derivative bw x=0 to x=1
he <- attr(dfun(x), "hessian")[ , , "x"] #computes 2nd derivative bw x=0 to x=1
k <- abs(he)/(1 + gr^2)^(3/2)

# k agrees with wolfram for x=1 and x=10

# ------------------------------------
# get arc length parameterization of x

param_poly <- parameterize(my_poly)

# seq includes zero, so we get 201 curvature measurements
iter <- seq(0, 1, by = 1/1000)
arcfun_list <- list()

arc <- pracma::arclength(param_poly, 1, 10)$length

for (i in seq_along(iter)) {
  arcfun_list[[i]] <- local({
    arc_sub <- iter[i] * arc
    function(u) pracma::arclength(circlefun, 0, u)$length - arc_sub
  })
}

root_find <- function(x) stats::uniroot(x, c(0, sqrt(3)/2))$root

x_param <- sapply(arcfun_list, root_find)


# -----------------------------------
# get y values for s-param'd x values


x2 <- x_param
y2 <- x_param^2
coord <- matrix(c(x2, y2), nrow=1001, ncol=2)


tangles_poly <-
  coo_angle_tangent(coord)

# key observation! tangles poly represents the tangent angles,
# NOT the *rate of change* of the tangent angles, which is what we want.

# -------------------------------------
# dissecting the tangent angle function
# methods(coo_angle_tangent)
# "the tangent angle is equal to the argument of the derivative of the curve"

  p <- nrow(coord) # the number of landmarks

  # coord[p,] is the last row (LM), coord[-p,] is every row except the last
  # rbind() part puts the last row as the first
  # coord - rbind() takes the raw LMs and subtracts the shifted LMs
  # tangvect therefore has deltaX as column1 and deltaY as column2
  # ie the change in x and y position from LM to LM
  tangvect <- coord - rbind(coord[p, ], coord[-p, ])

  # coord without first row *subtract*  coord without last row
  # therefore, first row is dropped (x_param = 1)
  tangvect <- coord[-1,] - coord[-nrow(coord),]

  # # trying to guess tangent for x=1
  # # dT = 0.02625940 -0.01107245
  # dT <- tangvect[2,] - tangvect[3,]
  #
  # # estimating dT for x=1 (tangvect[1,])
  # tangvect[1,] <- tangvect[2,] + dT


  # complex() converts real numbers to complex numbers:
  # the real component is column1 of coord
  # the imaginary component is column2 of coord
  #
  # The Arg() of a complex object is the angle inclined
  # from the real axis in the direction of the complex number
  # existing on the complex plane
  # the 'argument' of a complex number is arctan(y/x)

  # tet1 is a string of angles (radians) that are within the range -pi to +pi
  tet1 <-
    Arg(complex(
      real = tangvect[, 1],
      imaginary = tangvect[, 2]))

  # idea from: https://stackoverflow.com/questions/21698353/difference-between-neighboring-elements-of-a-vector-in-r
  # tet1 without first element *subtract* tet1 without last element
  # therefore, first element is dropped

  phi3 <-
    (tet1[-1] - tet1[-length(tet1)]) %>%
    sum(.)





# https://github.com/MomX/Momocs/blob/master/R/core-out-tfourier.R







# -------------
# unit circle
# K should be constant...

# parameterized unit circle
f <- function(x) c(x, (1-(x^2))^0.5)

# arc-length parameterization
iter <- seq(0, 1, by = 1/1000)
arcfun_list <- list()

arc <- pracma::arclength(f, 0, 0.9999)$length

for (i in seq_along(iter)) {
  arcfun_list[[i]] <- local({
    arc_sub <- iter[i] * arc
    function(u) pracma::arclength(f, 0.9999, u)$length - arc_sub

  })
}

root_find <- function(x) stats::uniroot(x, c(0, 0.9999))$root

# x-coords
x_param <- sapply(arcfun_list, root_find)

# x-coords (not param'd)
x <- seq(0, sqrt(2)/2, by =0.0001)
#y-coords
y <- (1-(x^2))^0.5

# plot unit circle
plot(x, y)

# coordinate matrix
circlecoord <- matrix(c(x,y), ncol =2)

# oddly, the first two rows have the same values..
circlecoord <- circlecoord[-1,]

#something weird:
# plot(diff(circlecoord[,2]))
# shows that the y values aren't evenly spaced
# i think this affects curvature calcs?



tangvect <- circlecoord[-1,] - circlecoord[-nrow(circlecoord),]

tet1 <-
  Arg(complex(
    real = tangvect[, 1],
    imaginary = tangvect[, 2]))

# ??? sum(phi3) gives 3.14???
phi3 <-
  (tet1[-1] - tet1[-length(tet1)])
  # abs() %>%
  # magrittr::multiply_by(180/pi)


# values are *very* close to 57.29579 which is 1 radian
# though they are negative...
# could take abs()

# ------------------------
# compare unit circle result
# with total_curvature()

circle_poly <- Momocs::npoly(circlecoord, 2)

total_curvature(f, c(0,1), 500)
