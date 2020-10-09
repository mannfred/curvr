library(Momocs)
library(pracma)
library(tidyverse)


# curvr approach
# set up mock LM data
x1 <- 1:10
y1<- 0.5*(x1^3.2)-99
mdat <- matrix(c(x1,y1), nrow=10, ncol=2)
my_poly <- Momocs::npoly(mdat, 2)

# point-wise K between 1 and 10
k <-
  total_curvature(my_poly, c(1, 10), 500)

# convert arc length to radians
alrad <- k$s * (pi/180)

alrad * k$total_k
# 0.4227657

# ---------------------
# tangles approach

x2 <- seq(1, 10, by = (10-1)/length(k$k))
y2 <- 0.5*(x2^3.2)-99

coords <- matrix(c(x2, y2), ncol =2)
plot(coords)

tangvect <- coords[-1,] - coords[-nrow(coords),]

tet1 <-
  Arg(complex(
    real = tangvect[, 1],
    imaginary = tangvect[, 2]))


phi <-
  (tet1[-1] - tet1[-length(tet1)])

# 0.315728
sum(phi)
