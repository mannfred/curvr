library(pracma) # for doing math (eg calculating arc length)


# ------------------------------------------
# estimate point-wise curvature along y=x^2

# x coordinates
x <- seq(1, 10, by=0.001)

# calculate y' and y" from 1 to 10
dfun <- deriv3(expression(x^2), "x", func=TRUE)

# y'
gr <- attr(dfun(x), "gradient")

#y"
he <- attr(dfun(x), "hessian")[ , , "x"]

# point-wise curvature
# this equation is coming from: https://en.wikipedia.org/wiki/Curvature#Graph_of_a_function
k <- (he)/ ((1 + gr^2)^1.5)

# values of k agree with Wolfram
# https://www.wolframalpha.com/widgets/view.jsp?id=477ce31923bd5cc8f9d1d0502062c445

# estimate arclength
polyfun <- function(t) c(t, t^2)
al <- pracma::arclength(polyfun, 1, 10)$length



k_fun <- function(x) 2/((1+(4*(x^2)))^1.5) * (sqrt(1+(4*(x^2))))

int <- integrate(k_fun, lower = 1, upper = 10)
# -----------------------------------------
# estimate tangent angles along y=x^2

# create matrix of coordinates
x2 <- seq(1, 10, by = 0.001) # but maybe it should be x2 <- seq(0, al, by = 0.001) ?
y2 <- x2^2

coords <- matrix(c(x2, y2), ncol =2)

# calculate tangent vector between adjacent coordinates
tangvect <- coords[-1,] - coords[-nrow(coords),]

# using Arg() to calculate the angle (arguement) between tangent vectors
tet1 <-
  Arg(complex(
    real = tangvect[, 1],
    imaginary = tangvect[, 2]))

# calculate the change in angle between coordinates
phi <-
  abs(tet1[-1] - tet1[-length(tet1)])


# total curvature = 0.413
sum(phi)


# -------------------
# scaling doesn't affect curvature
x <- seq(0.5, 5, by = 0.5)
y <- 2*(x^2)

# a fictional landmark dataset
mdat <- matrix(c(x,y), nrow=10, ncol=2)

# fit second-order polynomial to landmarks
my_poly <- Momocs::npoly(mdat, 2)

total_curvature(my_poly, c(0.5,5), 500)

