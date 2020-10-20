library(Momocs)
library(pracma)
library(tidyverse)

# be careful! Momocs polys are not that accurate for calculating curvature..
# curvr approach
# set up mock LM data
x1 <- 1:10
y1<- x1^2.18
mdat <- matrix(c(x1,y1), nrow=10, ncol=2)
my_poly <- Momocs::npoly(mdat, 3)

# draw
opi <- npoly_i(my_poly)
coo_plot(opi, xlim)

# point-wise K between 1 and 10
k <-
  total_curvature(my_poly, c(1, 10), 1000)

# pracma::arclength()
# computes arclength in radians
# (this was tested on a unit circle)

# multiply by arc length
# total curvature is expressed in degrees
k$total_k * (180/pi) * k$s


# ---------------------
# tangles approach

x2 <- seq(0, 1, by = (10-1)/length(k$k))
y2 <- (1-(x2^2))^0.5

coords <- matrix(c(x2, y2), ncol =2)


tangvect <- coords[-1,] - coords[-nrow(coords),]

tet1 <-
  Arg(complex(
    real = tangvect[, 1],
    imaginary = tangvect[, 2]))


phi <-
  abs(tet1[-1] - tet1[-length(tet1)])

# convert to degrees
phideg <- phi * (180/pi)

# integrate; total curvature is expressed in degrees
sum(phideg)
