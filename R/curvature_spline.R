#' Calculate total curvature from smoothing  splines.
#'
#'
#' @param model is a \code{lm} object fitted to the predicted values
#' of the first derivative of a \code{smooth.spline} object.
#' See examples below.
#'
#' @param x_range the lower and upper x-value bounds to
#' calculate curvature. Concatenate the lower and upper bounds using
#' \code{c()}, E.g. for lower = 1 and upper = 10, type \code{c(1,10)}.
#'
#'
#' @return a numeric vector length 1 indicating the total curvature in radians.
#'
#' @examples
#'
#' @importFrom dplyr %>%
#'
#' @export

curvature_spline <- function(landmark_matrix, x_range) {

  # extract/separate x and y coords
  x_coords <- landmark_matrix[, 1]
  y_coords <- landmark_matrix[, 2]

  # check that for every x there is a y
  if (length(x_coords) != length(y_coords)) {
    stop("every x coordinate must have a corresponding y coordinate")
  }

  # fit a spline to landmark coordinates
  s0 <- smooth.spline(landmark_matrix)

  # first deriv values
  s1 <- predict(s0, deriv = 1)

  # fit spline func to first deriv values
  s1func <- splinefun(x = s1$x, y = s1$y)

  # second deriv func (1st deriv of s1)
  s2func <- splinefun(x = s1$x, y = s1func(s1$x, deriv = 1))

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
