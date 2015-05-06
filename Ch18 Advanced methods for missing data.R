#-----------------------------------#
# R in Action (2nd ed): Chapter 18  #
# Advanced methods for missing data #
# requires packages VIM, mice       #
# install.packages(c("VIM", mice))  #
#-----------------------------------#

par(ask=TRUE)


# load the dataset
data(sleep, package="VIM")


# list the rows that do not have missing values
sleep[complete.cases(sleep),]


# list the rows that have one or more missing values
sleep[!complete.cases(sleep),]


# tabulate missing values patters
library(mice)
md.pattern(sleep)


# plot missing values patterns
library("VIM")
aggr(sleep, prop=FALSE, numbers=TRUE)
matrixplot(sleep)
marginplot(sleep[c("Gest","Dream")], pch=c(20), 
           col=c("darkgray", "red", "blue"))


# use correlations to explore missing values
x <- as.data.frame(abs(is.na(sleep)))
head(sleep, n=5)
head(x, n=5)
y <- x[which(apply(x,2,sum)>0)]
cor(y)
cor(sleep, y, use="pairwise.complete.obs")


# complete case analysis (listwise deletion)
options(digits=1)
cor(na.omit(sleep))
fit <- lm(Dream ~ Span + Gest, data=na.omit(sleep))
summary(fit)


# multiple imputation
options(digits=3)
library(mice)
data(sleep, package="VIM")
imp <- mice(sleep, seed=1234)
fit <- with(imp, lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)
imp



