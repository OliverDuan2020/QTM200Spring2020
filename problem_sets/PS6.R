#####################
# load libraries
# set wd
# clear global .envir
###Yuncong(Oliver) Duan
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

lapply(c(),  pkgTest)

# set working directory

getwd()
setwd("~/Desktop/GitHub/QTM200Spring2020/problem_sets/PS6")

#####################
# Problem 1
#####################

cholesterol <- read.csv("https://raw.githubusercontent.com/jeffreyziegler/QTM200Spring2020/master/problem_sets/PS6/cholesterol.csv")


##1)
AdditiveModel <- lm(cholCat ~ sex + fat, data=cholesterol)
summary(AdditiveModel)
confint(AdditiveModel,level=0.95)

H0: there is never a relationship between cholesterol ~ sex + fat
H1: The relationship does exist
P value <2.2e-16
The probability that there is never going to be a relationship between response variable and explanatory variable is very low
There is a relationship between cholesterol ~ sex +fat


##2)

cholesterol = -0.1303597 + 0.1894* sex + 0.0082466* fat
##a)
For females, when their fat increases by 1 gram, the probability that they will be in the high choleserol group will increase 0.8%

##b)
For males, when their fat increases by 1 gram, the probability that they will be in the high cholesterol group will increase 18.8%

##c)
cholesterol = -0.1303597 + 0.1894* 0 + 0.0082466* 100 = 0.6943003
The probability of a woman with a fat intake of 100 grams per day being in the high cholesterol group is 69.43%

##d)
InteractionModel <- lm(cholCat ~ sex + fat+ sex*fat, data=cholesterol)
summary(InteractionModel)
No, by incorporating a new interaction term, the coefficients for sex and fat are not changed and the explanatory power of my regression model is not increased compared with my model without the interaction term. Thus, there is no need to incorporate an interactive term.

#####################
# Problem 2
#####################

gdpChange <- read.csv("https://raw.githubusercontent.com/jeffreyziegler/QTM200Spring2020/master/problem_sets/PS6/gdpChange.csv")

str(gdpChange)


gdpChange$out<-relevel(gdpChange$GDPWdiff, ref="no change")

install.packages("nnet")
library(nnet)

##1)
myModel<-multinom(out~REG+OIL, data=gdpChange,Hess = TRUE)
summary(myModel)

##2)
install.packages("MASS")
library(MASS)
gdpChange$out<-ordered(gdpChange$out,levels=c("negative","no change","positive"))
polr(out~REG+OIL,gdpChange)
