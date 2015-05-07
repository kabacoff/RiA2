#' @title Print multiple comparisons
#'
#' @description
#' \code{print.oneway} prints pairwise group comparisons.
#'
#' @details
#' This function prints Wilcoxon pairwise multiple comparisons created
#' by the \code{\link{oneway}} function.
#' 
#' @param x an object of class \code{oneway}.
#' @param ... additional arguments passed to the function.
#' @method print oneway
#' @export
#' @return the input object is returned silently.
#' @author Rob Kabacoff <rkabacoff@@statmethods.net>
#' @examples
#' results <- oneway(hlef ~ region, life)
#' print(results)
print.oneway <- function(x, ...){
  if (!inherits(x, "oneway"))       
    stop("Object must be of class 'oneway'")
  
  cat("data:", x$vnames[1], "by", x$vnames[2], "\n\n")  
  cat("Multiple Comparisons (Wilcoxon Rank Sum Tests)\n")
  cat(paste("Probability Adjustment = ", x$method, "\n", sep=""))
  
  print(x$wmc,  ...)
}
