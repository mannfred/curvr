library(curvr)
library(Momocs)

# set up mock LM data
x <- 1:10
y <- x^2
mdat <- matrix(c(x,y), nrow=10, ncol=2)
my_poly <- Momocs::npoly(mdat, 2)

# point-wise K between 1 and 10
k <-
  total_curvature(my_poly, c(1, 10), 200)

# all K values from curvr check out with
# https://www.wolframalpha.com/input/?i=curvature+of+y%3Dx%5E2+at+x%3D10

# -------------------------
# tangent angle

# first, interpolate between our 10 LMs
int_mdat <-
  coo_interpolate(mdat, 400) %>%
  .[1:201,] # coo_interpolate tries to close the curve so take half of the points

# interpolating works pretty well!
plot(int_mdat)


# tangent angle in radians
# modulo 2pi
tangles <-
  coo_angle_tangent(int_mdat)

# but tangent angles and K aren't the same..
plot(tangles, k$k)



# ------------------------------------
# get arc length parameterization of x

param_poly <- curvr::parameterize(my_poly)

iter <- seq(0, 1, by = 1/200)
arcfun_list <- list()

arc <- pracma::arclength(param_poly, 1, 10)$length

for (i in seq_along(iter)) {
  arcfun_list[[i]] <- local({
    arc_sub <- iter[i] * arc
    function(u) pracma::arclength(param_poly,
                                  1, u)$length - arc_sub
  })
}

root_find <- function(x) stats::uniroot(x, c(1,10))$root

x_param <- sapply(arcfun_list, root_find)

# ----------------------------
# alternative to interpolation

polyfun <- function(x) x^2
x2 <- x_param
y2 <- polyfun(x2)
coord <- matrix(c(x2, y2), nrow=201, ncol=2)

tangles_poly <-
  coo_angle_tangent(coord)


# -------------------------------------
# dissecting the tangent angle function


  p <- nrow(coord) # the number of landmarks

  # coo[p,] is the last row (LM), coo[-p,] is every row except the last
  # rbind() part puts the last row as the first
  # coo - rbind() takes the LMs and subtracts the shifted LMs
  # tangvect therefore has deltaX as column1 and deltaY as column2
  # ie the change in x and y position from LM to LM
  tangvect <- coord - rbind(coord[p, ], coord[-p, ])

  # coord without first row *subtract*  coord without last row
  # therefore, first row is dropped
  tangvect <- coord[-1,] - coord[-nrow(coord),]

  # trying to guess tangent for x=1
  # dT = 0.02625940 -0.01107245
  dT <- tangvect[2,] - tangvect[3,]

  # estimating dT for x=1 (tangvect[1,])
  tangvect[1,] <- tangvect[2,] + dT


  # complex() converts real numbers to complex numbers:
  # the real component is column1 of coord
  # the imaginary component is column2 of coord
  #
  # The Arg() of a complex object is the angle inclined
  # from the real axis in the direction of the complex number
  # existing on the complex plane
  # the 'argument' of a complex number is arctan(y/x)

  # tet1 is a string of angles (radians)
  tet1 <-
    Arg(complex(
      real = tangvect[, 1],
      imaginary = tangvect[, 2]))

  # idea from: https://stackoverflow.com/questions/21698353/difference-between-neighboring-elements-of-a-vector-in-r
  # tet1 without first element *subtract* tet1 without last element
  # therefore, first element is dropped
  phi3 <- (tet1[-1] - tet1[-length(tet1)]) * (180/pi)

  # looks 1:1
  plot(k$k[-(c(1,201))], phi3)

# https://github.com/MomX/Momocs/blob/master/R/core-out-tfourier.R

# ---------------------
# How does Arg() work?

 # http://www.johnmyleswhite.com/notebook/2009/12/18/using-complex-numbers-in-r/

  z <- complex(real = 0, imaginary = 1)

  Mod(z)
  # [1] 1

  Arg(z)
  # [1] 1.570796

  pi / 2
  # [1] 1.570796

  Arg(z) * (180/pi)
  # 90 degrees

  # This corresponds to the intuition that I should be
  # at a distance 1 from the origin and an angle of pi / 2 or 90 degrees

  # So, Arg() is clearly sensible. The problem either lies at the
  # tangvect stage or the calculation of phi..
