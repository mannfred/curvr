library(Momocs)
library(pracma)
library(tidyverse)


# curvr approach
# set up mock LM data
x1 <- 1:10
y1<- x1^2
mdat <- matrix(c(x1,y1), nrow=10, ncol=2)
my_poly <- Momocs::npoly(mdat, 2)

# point-wise K between 1 and 10
k <-
  total_curvature(my_poly, c(1, 10), 500)

# pracma::arclength()
# computes arclength in radians
# (this was tested on a unit circle)

# multiply by arc length
# total curvature is expressed in degrees
k$total_k * k$s


# ---------------------
# tangles approach

x2 <- seq(1, 10, by = (10-1)/length(k$k))
y2 <- x2^2

coords <- matrix(c(x2, y2), ncol =2)
plot(coords)

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
