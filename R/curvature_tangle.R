#' Calculate total curvature by taking the derivative of
#' the Zahn and Roskies tangent angle function
#'
#'
#' @param function a single variable function of x.
#' E.g. for the polynomial function y=x^2 simply type \code{y^2}.
#'
#' @param x_range the lower and upper x-value bounds to
#' calculate curvature. Concatenate the lower and upper bounds using
#' \code{c()}, E.g. for lower = 1 and upper = 10, type \code{c(1,10)}.
#'
#' @param subdiv number of subdivisions made
#' within \code{x_range}. Curvature is calculated at every
#' subdivision and summed to compute total curvature.
#' Default is 1000. The same number of subdivisions will be
#' applied to all curves, regardless of arclength.
#'
#' @return a numeric vector length 1 indicating the total curvature in radians.
#'
#' @examples
#'
#' @importFrom dplyr %>%
#'
#' @export

curvature_tangle <- function(coo_landmarks, interpolation = 500) {

  # create matrix of coordinates
  x2 <- seq(1, 10, by = 0.001)
  y2 <- x2^2

  coords <- matrix(c(x2, y2), ncol = 2)

  # calculate tangent vector between adjacent coordinates
  tangvect <- coords[-1, ] - coords[-nrow(coords), ]

  # using Arg() to calculate the angle (arguement) between tangent vectors
  tet1 <-
    Arg(complex(
      real = tangvect[, 1],
      imaginary = tangvect[, 2]
    ))

  # calculate the change in angle between coordinates
  phi <-
    abs(tet1[-1] - tet1[-length(tet1)])


  # total curvature = 0.413
  sum(phi)
}
