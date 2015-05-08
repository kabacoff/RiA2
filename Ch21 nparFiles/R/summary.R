#' @title Summarize oneway nonparametric analyses
#'
#' @description
#' \code{summary.oneway} summarizes the results of a oneway 
#' nonparametric analysis.
#'
#' @details
#' This function prints a summary of analyses produced by
#' the \code{\link{oneway}} function. This includes descriptive
#' statistics by group, an omnibus Kruskal-Wallis test, and 
#' Wilcoxon pairwise multiple comparisons.
#' 
#' @param object an object of class \code{oneway}.
#' @param ... additional parameters.
#' @method summary oneway
#' @export
#' @return the input object is returned silently.
#' @author Rob Kabacoff <rkabacoff@@statmethods.net>
#' @examples
#' results <- oneway(hlef ~ region, life)
#' summary(results)
summary.oneway <- function(object, ...){
  if (!inherits(object, "oneway")) 
    stop("Object must be of class 'oneway'")
  
  if(!exists("digits")) digits <- 4L
  
  kw <- object$kw
  wmc <- object$wmc
  
  cat("data:", object$vnames[1], "on", object$vnames[2], "\n\n")
  
  cat("Omnibus Test\n")                        
  cat(paste("Kruskal-Wallis chi-squared = ", 
            round(kw$statistic,4), 
            ", df = ", round(kw$parameter, 3), 
            ", p-value = ", 
            format.pval(kw$p.value, digits = digits), 
            "\n\n", sep=""))
  
  cat("Descriptive Statistics\n")     
  print(object$sumstats, ...)
  
  
  wmc$stars <- " "                  
  wmc$stars[wmc$p <   .1] <- "."
  wmc$stars[wmc$p <  .05] <- "*"
  wmc$stars[wmc$p <  .01] <- "**"
  wmc$stars[wmc$p < .001] <- "***"
  names(wmc)[which(names(wmc)=="stars")] <- " "                          
  
  cat("\nMultiple Comparisons (Wilcoxon Rank Sum Tests)\n")    
  cat(paste("Probability Adjustment = ", object$method, "\n", sep=""))
  print(wmc, ...)
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}
