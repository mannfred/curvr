library(pracma) # for doing math (eg calculating arc length)


# -----------------------------------------------
# estimate point-wise curvature along unit circle

# x coordinates
x <- seq(0, sqrt(3)/2, by=0.001)

# calculate y' and y" from 0 to sqrt(3)/2
dfun <- deriv3(expression(sqrt(1-(x^2))), "x", func=TRUE)

# y'
gr <- attr(dfun(x), "gradient")

# y"
he <- attr(dfun(x), "hessian")[ , , "x"]

# point-wise curvature (=1 at every point on unit circle)
k <- abs(he)/ ((1 + gr^2)^1.5)
# agrees with Wolfram
# # https://www.wolframalpha.com/widgets/view.jsp?id=477ce31923bd5cc8f9d1d0502062c445

# arclength between 0 and sqrt(3)/2
circlefun <- function(t) c(t, sqrt(1-(t^2)))
al <- arclength(circlefun, 0, sqrt(3)/2)$length

# for every piece (i) of arclength (al/length), multiply
# that piece with its corresponding curvature (k_i)
# total curvature = pi/3 (expected)
(al/(length(x)-1)) * sum(k)



# -----------------------------------------
# estimate tangent angles along unit circle

# create matrix of coordinates
x2 <- seq(0, sqrt(3)/2, by = 0.001)
y2 <- sqrt(1-(x2^2))

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


# arclength
circlefun <- function(t) c(t, sqrt(1-(t^2)))
arc <- arclength(circlefun, 0, sqrt(3)/2)$length

# total curvature = pi/3
sum(phi)

