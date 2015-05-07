#-------------------------------------------------------------------#
# R in Action (2nd ed): Chapter 21                                  #
# Creating a package                                                #
# requires packages roxygen2                                        #
# install.packages("roxygen2", depend=TRUE)                         #
# Windows users will need                                           #
#   rtools.exe (http://cran.r-project.org/bin/windows/Rtools/)      #
#   MikTeX (http://miktex.org/)                                     #
# Mac users will need                                               #
#   MacTeX (http://www.tug.org/mactex/)                             #
# Linux users should just need the roxygen2 package                 #
#-------------------------------------------------------------------#

# Install the npar package
pkg <- "npar_1.0.tar.gz"
loc <- "http://www.statmethods.net/RiA"
url <- paste(loc, pkg, sep="/")
download.file(url, pkg)
install.packages(pkg, repos=NULL, type="source")

# Explore the life dataset
library(npar)
hist(life$hlef, xlab="Healthy Life Expectacy (years) at Age 65",
     main="Distribution of Healthy Life Expectancy for Women", 
     col="grey", breaks=10)
dotchart(hlef ~ region, life, col="darkgrey", vertical=TRUE,
         xlab="US Region",
         ylab="Healthy Life Expectancy (years) at Age 65",
         main="Distribution of HLE Estimates by Region")


# Listing 21.1 - Comparison of HLE estimates with the npar package
results <- oneway(hlef ~ region, life)
summary(results)
plot(results, col="lightblue", main="Multiple Comparisons",  
     xlab="US Region", 
     ylab="Healthy Life Expectancy (years) at Age 65")


# source code for the package can be 
# downloaded from
http://www.statmethods.net/RiA/nparFiles.zip