#' Calculate total curvature by taking the derivative of
#' the Zahn and Roskies tangent angle function
#'
#'
#' @param landmark_matrix is a \code{matrix} object with \code{[,1]}
#' containing the x landmark coordinates and \code{[,2]} containing
#' the y landmark coordinates.
#'
#' @param iterations is the number of times to run the spline interpolation procedure.
#' The more times this is run, the more the specimen will be sampled. Every 1-2 iterations
#' adds an order of magnitude of interpolated landmarks.
#'
#' @return a numeric vector length 1 indicating the total curvature in radians.
#'
#' @examples
#'
#' # a landmark matrix describing a 1/8 unit circle
#' x1 <- seq(0, sqrt(2)/2, length.out=9)
#' y1 <- sqrt(1-x1^2)
#' mdat1 <- matrix(c(x1, y1), nrow = length(x1), ncol = 2)
#'
#' # total curvature should be approximately pi/4
#' abs(curvature_tangle(mdat1, 10))
#'
#' @importFrom dplyr %>%
#'
#' @export

curvature_tangle <- function(landmark_matrix, iterations) {

  mdat <- landmark_matrix
  iter <- iterations
  sp_list <- list()

  sp_list[[1]] <- spline(mdat[,1], mdat[,2], method='fmm') # first interpolation


  for (i in 2:iter) {
    sp_list[[i]] <- spline(sp_list[[i-1]]$x, sp_list[[i-1]]$y, method='fmm') # iterations 2 to n
  }


  coords <- matrix(c(sp_list[[iter]]$x, sp_list[[iter]]$y), nrow = length(sp_list[[iter]]$x), ncol = 2)

  # calculate tangent vector between adjacent coordinates
  tangvect <- diff(coords)

  # using Arg() to calculate the angle (argument) between tangent vectors
  tet1 <-
    complex(real = tangvect[, 1], imaginary = tangvect[, 2]) %>%
    Arg()

  # calculate the change in angle between coordinates
  dphi <- abs(diff(tet1))

  # total curvature
  totalK <- sum(dphi)

  return(totalK)
}
