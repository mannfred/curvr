library(Momocs)
library(pracma)

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


# ------------------------------------
# get arc length parameterization of x

param_poly <- parameterize(my_poly)

# seq includes zero, so we get 201 curvature measurements
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

# -----------------------------------
# get y values for s-param'd x values

polyfun <- function(x) x^2
x2 <- x_param
y2 <- polyfun(x2)
coord <- matrix(c(x2, y2), nrow=201, ncol=2)


tangles_poly <-
  coo_angle_tangent(coord)

# key observation! tangles poly represents the tangent angles,
# NOT the *rate of change* of the tangent angles, which is what we want.

# -------------------------------------
# dissecting the tangent angle function
# methods(coo_angle_tangent)

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

  # tet1 is a string of angles (radians)
  tet1 <-
    Arg(complex(
      real = tangvect[, 1],
      imaginary = tangvect[, 2]))

  # idea from: https://stackoverflow.com/questions/21698353/difference-between-neighboring-elements-of-a-vector-in-r
  # tet1 without first element *subtract* tet1 without last element
  # therefore, first element is dropped

  phi3 <- (tet1[-1] - tet1[-length(tet1)]) * (180/pi)

  # looks 1:1, but values of phi3 are ~1/2 of k
  plot(k$k[-(c(1,201))], phi3)

# https://github.com/MomX/Momocs/blob/master/R/core-out-tfourier.R




# ----------------
# How does Arg() work?
# Are arguements directly translatable to degrees? How does modulus play in?
# searching for .Primitives with jennybc
# https://github.com/jennybc/access-r-source

# pryr can take us to the GitHub page for Arg()
library(pryr)
pryr::show_c_source(.Primitive(Arg(x)))

# https://github.com/search?q=SEXP%20attribute_hidden%20do_cmathfuns+repo:wch/r-source&type=Code

# https://github.com/wch/r-source/blob/f9266766088b7f0d73cec794c6e2e9e95abfa138/src/main/names.c
# /* printname	c-entry		offset	eval	arity   	pp-kind	    precedence	rightassoc
# {"Arg",		do_cmathfuns,	4,	     1,	      1,   {PP_FUNCALL,  PREC_FN,	 0}}

# line ~285 uses "carg function" and "atan2" (C code)

# from https://en.cppreference.com/w/c/numeric/math/atan2
# If no errors occur, the arc tangent of y/x
# in the range [-pi ; +pi] radians, is returned.
# meaning clockwise and counterclockwise to 180

# -------------------------
# tangent angle

# # first, interpolate between our 10 LMs
# int_mdat <-
#   coo_interpolate(mdat, 400) %>%
#   .[1:201,] # coo_interpolate tries to close the curve so take half of the points
#
# # interpolating works pretty well!
# plot(int_mdat)
#
#
# # tangent angle in radians
# # modulo 2pi
# tangles <-
#   coo_angle_tangent(int_mdat)
#
# # but tangent angles and K aren't the same..
# plot(tangles, k$k)

