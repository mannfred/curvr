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




# ------------------
# Sally's spirals

f <-
  function(x) {
  c(
    sqrt((4/3)*x)*cos((4/3)*x) + 20,
     sin((4/3)*x) + 20
    )
}

# multiply the values by 3/4.91 to scale by arclength (see Sally's notes)
x3 <- c(0, 0.5, 1, 1.5, 2)
y3 <- f(x3) * (3/4.91882)

spiral <- matrix(c(y3[1:5], y3[6:10]) , nrow=5, ncol=2)
plot(spiral)

# find out what params give arc length = 3
arclength(f, 0, 1.80002)


# ------------------
# now compute curvature between 0 and 1.8
x4 <- seq(0, 3, by=0.001)
y4 <- f(x4) * (3/4.91882)

spiral <- matrix(c(y4[1:3001], y4[3002:6002]) , nrow=3001, ncol=2)
plot(spiral)


tangvect <- spiral[-1,] - spiral[-nrow(spiral),]


# need to address discontinuity
tet1 <-
  Arg(complex(
    real = tangvect[, 1],
    imaginary = tangvect[, 2]))

# shift by pi at discontinuity (see Sally's notes)
tet1[1178:3000] <- c(tet1[1178:3000] + 2*pi)

# remove a weird data point
tet1 <- tet1[-1178]

phi <-
  (tet1[-1] - tet1[-length(tet1)])

# agrees with Sally's mathematica result
sum(phi)
