#' @title Hypothetical weight-length data for testing PSD and relative weight functions
#'
#' @description Hypothetical weight-length and associated data. These data are useful for testing PSD and relative weight functions (e.g., \code{\link{psdAdd}} and \code{\link{wrAdd}}).
#'
#' @name PSDWRtest
#'
#' @docType data
#'
#' @format A data frame of many observations on the following 5 variables:
#'  \describe{
#'    \item{species}{Species name}
#'    \item{location}{Broad location of capture}
#'    \item{len}{Length in mm}
#'    \item{wt}{Weight in g}
#'    \item{sex}{Sex as \code{F} for female, \code{M} for male, or \code{U} or \code{NA} for unknown or unrecorded}
#'  }
#'
#' @section Topic(s):
#'  \itemize{
#'    \item Size structure
#'    \item Proportional size structure
#'    \item Relative stock density
#'    \item Proportional stock density
#'    \item Relative weight
#'    \item Standard weight
#'    \item Condition
#'  }
#'
#' @concept Size Structure
#' @concept PSD
#' @concept Condition
#' @concept Relative Weight
#' @concept Standard Weight
#'
#' @seealso \code{\link{psdAdd}}, \code{\link{psdCalc}}, and \code{\link{wrAdd}}
#'
#' @keywords datasets
#'
#' @examples
#' str(PSDWRtest)
#' peek(PSDWRtest,n=20)
#' unique(PSDWRtest$species)
#'
NULL
