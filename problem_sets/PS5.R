#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("faraway"),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS5")


#####################
# Problem 1
#####################

# load data
gamble <- (data=teengamb)
# run regression on gamble with specified predictors
model1 <- lm(gamble ~ sex + status + income + verbal, gamble)

install.packages("car")
library("car")

#a)
plot(residuals(model1) ~ fitted(model1), data=gamble)
abline(h=0)

# Use absolute residuals
plot(abs(residuals(model1)) ~ fitted(model1), data=gamble)
abline(h=0)

#b)
qqPlot(model1)
#What does the blue line mean??

#c)
hatvalues(model1)
plot(hatvalues(model1),pch=16,cex=2)
abline(h=2*3/47,lty=2)
abline(h=3*3/47,lty=3)
identify(1:47,hatvalues(model1),row.names(gamble))

#d)
outlierTest(model1)
cooks.dist <- cooks.distance(model1)
plot(cooks.dist)

#e)
influencePlot(model1,
              sub="Circle size is proportial to Cook's Distance")
