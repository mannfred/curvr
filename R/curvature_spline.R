#' Calculate total curvature from smoothing or interpolating splines.
#'
#'
#' @param landmark_matrix is a \code{matrix} object with \code{[,1]}
#' containing the x landmark coordinates and \code{[,2]} containing
#' the y landmark coordinates.
#'
#' @param x_range the lower and upper x-value bounds to
#' calculate curvature. Concatenate the lower and upper bounds using
#' \code{c()}, E.g. for lower = 1 and upper = 10, type \code{c(1,10)}.
#'
#' @param type either 'ip' for an interpolating spline or 'smooth' for a
#' smoothing spline. Uses \code{stats::spline()} or \code{stats::smooth.spline()}, respectively,
#' for curve fitting and estimating the derivatives. Default is \code{type = 'smooth'}.
#' See: ?spline and ?smooth.spline for details.
#'
#' @return a numeric indicating the total curvature in radians.
#'
#' @examples
#'
#' # a landmark matrix describing a segment of the unit circle#'
#' x <- seq(0, 1, by = 0.01)
#' y <- sqrt(1-x^2)
#' mdat <- matrix(c(x, y), nrow = 101, ncol = 2)
#'
#' # total curvature between x=0 and x=sqrt(2)/2 should be approximately pi/4
#' abs(curvature_spline(mdat, c(0, sqrt(2)/2), type='smooth'))
#'
#' @importFrom dplyr %>%
#' @importFrom stats smooth.spline predict splinefun spline integrate
#'
#' @export

curvature_spline <- function(landmark_matrix, x_range, type = 'smooth', remove_local_K=NULL) {

  # extract/separate x and y coords
  x_coords <- landmark_matrix[, 1]
  y_coords <- landmark_matrix[, 2]

  # check that for every x there is a y
  if (length(x_coords) != length(y_coords)) {
    stop("every x coordinate must have a corresponding y coordinate")
  }

  if (type == 'smooth'){
    # fit a spline to landmark coordinates
    s0 <- smooth.spline(landmark_matrix)

    # first deriv values
    s1 <- predict(s0, deriv = 1)

    # fit spline func to first deriv values
    s1func <- splinefun(x = s1$x, y = s1$y)

    # second deriv func (1st deriv of s1)
    s2func <- splinefun(x = s1$x, y = s1func(s1$x, deriv = 1))

  } else if (type == 'ip'){

    # compute coordinates from a cubic spline fit
    s0 <- spline(landmark_matrix)

    # create a spline function from coordinates
    s0func <- splinefun(s0$x, s0$y)

    # estimate y coords of first derivative
    s1 <- s0func(s0$x, deriv = 1)

    # create a function for first derivative
    s1func <- splinefun(s0$x, s1)

    # create a function for second derivative
    s2func <- splinefun(x = s0$x, y = s1func(s0$x, deriv = 1))

  } else {

    stop("spline type must be 'ip' or 'smooth'")

  }

  # define K * ds
  k_fun <- function(x) {
    f1 <- s1func
    f2 <- s2func
    ((f2(x)) / ((1 + (f1(x)^2))^1.5)) * (sqrt(1 + (f1(x))^2))
  }

  # compute integral of K*ds
  Ktot <- integrate(k_fun, lower = x_range[1], upper = x_range[2])$value


  # remove local curvature anomalies
  y <- sapply(seq(x_range[1], x_range[2], by = 0.01), k_fun)
  Ki <- diff(y)

  if (is.null(remove_local_K) == FALSE) {
    for (i in 1:length(Ki)) {
      if (abs(Ki[i]) > abs(remove_local_K*Ktot)) {Ki[i] <- 0}
      else {Ki[i] <- Ki[i]}
    }
  }

  curvature <- list(Ktot = Ktot, Ktot_without_local = sum(Ki), Ki = Ki )
  return(curvature)
}
