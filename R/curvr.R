#' curvr: Calculate Point-Wise Curvature From Landmarked Specimens
#'
#' Calculate point-wise curvature from polynomial functions
#' fitted to landmarked specimens. \code{curvr} is designed
#' to pipe from \code{Momocs}, where you can fit polynomial functions
#' to landmark data. Future versions to incorporate other curve-fitting functions.
#'
#' @section curvr functions:
#' The main function of \code{curvr} is \code{total_curvature()} - it calculates point-wise
#' curvature along bounded polynomials fitted to curved specimens (e.g. corolla tubes, bird bills).
#'
#' Calculating point-wise curvature, K, involves parameterizing your polynomial function
#' by arclength and computing the tangent directions along the curve. The helper function
#' \code{parameterize()} converts polynomial objects from \code{Momocs} into t-parameterized
#' polynomials readable by \code{pracma::arclength()}. \code{as_function()} computes the
#' y coordinates of our polynomial once the x-coordinates of the arclength-parameterized
#' function are known. \code{as_expression()} converts a \code{Momocs} polynomial to an
#' expression so that derivatives can be computed using \code{stats::deriv3()}.
#'
#' @docType package
#' @name curvr
NULL
