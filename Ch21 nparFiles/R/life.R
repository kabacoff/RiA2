#' @title Healthy Life Expectancy at Age 65
#' 
#' @description A dataset containing the healthy life expectancy (expected
#' years of life in good health) at age 65, by US state in 2007-2009. 
#' Estimates are reported separately for men and women. 
#' 
#' @docType data
#' @keywords datasets
#' @name life
#' @usage life
#' @format A data frame with 50 rows and 4 variables. The variables
#' are as follows:
#' \describe{
#'   \item{region}{A factor with 5 levels (North Central, Northeast, South, West)}
#'   \item{state}{A factor with the 2-letter ISO codes for the 50 US states} 
#'   \item{hlem}{Healthy life expectancy for men in years}
#'   \item{hlef}{Healthy life expectancy for women in years}
#' }
#' @source The \code{hlem} and \code{hlef} data were obtained from the Center for Disease Control 
#' and Prevention \emph{Morbidity and Mortality Weekly Report} at
#' \url{http://www.cdc.gov/mmwr/preview/mmwrhtml/mm6228a1.htm?s_cid=mm6228a1_w}.
#' The \code{region} variable was added from the 
#' \code{\link[datasets]{state.region}} dataset.
NULL