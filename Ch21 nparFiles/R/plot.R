#' @title Plot nonparametric group comparisons
#'
#' @description
#' \code{plot.oneway} plots nonparametric group comparisons.
#'
#' @details
#' This function plots nonparametric group comparisons
#' created by the \code{\link{oneway}} function using
#' annotated side by side boxplots. Medians and
#' sample sizes are placed at the top of the chart.
#' The overall median is represented by a horizontal
#' dashed line.
#' 
#' @param x an object of class \code{oneway}.
#' @param ... additional arguments passed to the 
#' \code{\link{boxplot}} function.
#' @method plot oneway
#' @export
#' @return NULL
#' @author Rob Kabacoff <rkabacoff@@statmethods.net>
#' @examples
#' results <- oneway(hlef ~ region, life)
#' plot(results, col="lightblue", main="Multiple Comparisons",
#'      xlab="US Region", ylab="Healthy Life Expectancy at Age 65")
plot.oneway <- function(x, ...){                   
  if (!inherits(x, "oneway")) 
    stop("Object must be of class 'oneway'")
  data <- x$data                                    
  y <- data[,1]
  g <- data[,2]
  stats <- x$sumstats
  lbl <- paste("md=", stats[2,], "\nn=", stats[1,], sep="")
  opar <- par(no.readonly=TRUE)
  par(mar=c(5,4,8,2))
  boxplot(y~g,  ...)
  abline(h=median(y), lty=2, col="darkgray")
  axis(3, at=1:length(lbl), labels=lbl, cex.axis=.9)
  par(opar)
}
