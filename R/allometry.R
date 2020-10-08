library(Momocs)
library(pracma)
library(tidyverse)

# set up mock LM data
x <- 1:10
y <- x^2
mdat <- matrix(c(x,y), nrow=10, ncol=2)
my_poly <- Momocs::npoly(mdat, 2)

# point-wise K between 1 and 10
k <-
  total_curvature(my_poly, c(1, 10), 500)


# --------------------
x2 <- x/2
y2 <- 2*(x2^2)
mdat2 <- matrix(c(x2,y2), nrow=10, ncol=2)
my_poly2 <- Momocs::npoly(mdat2, 2)

# plot
plot(mdat, pch=16)
points(mdat2, pch=16, col='red')


# point-wise K
k2 <-
  total_curvature(my_poly2, c(0.5, 5), 500)

