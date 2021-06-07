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
#' @importFrom dplyr %>%
#'
#' @export

curvature_spline <- function(landmark_matrix, x_range, type = 'smooth') {

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
  return(Ktot)
}
