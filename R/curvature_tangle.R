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

curvature_tangle <- function(landmark_matrix) {

  mdat <- landmark_matrix
  ip1 <- spline(mdat[,1], mdat[,2], method='fmm') #first interpolation
  ip2 <- spline(ip1$x, ip1$y, method='fmm') #second interpolation
  ip3 <- spline(ip2$x, ip2$y, method='fmm') # third interpolation

  coords <- matrix(c(ip3$x, ip3$y), nrow = length(ip3$x), ncol = 2)

  # calculate tangent vector between adjacent coordinates
  tangvect <- coords[-1, ] - coords[-nrow(coords), ]

  # using Arg() to calculate the angle (argument) between tangent vectors
  tet1 <-
    Arg(complex(
      real = tangvect[, 1],
      imaginary = tangvect[, 2]
    ))

  # calculate the change in angle between coordinates
  dphi <-
    abs(tet1[-1] - tet1[-length(tet1)])


  # total curvature
  totalK <- sum(dphi)

  return(totalK)
}
