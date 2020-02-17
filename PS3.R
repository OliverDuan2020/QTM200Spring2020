#####################
# load libraries
# set wd
# clear global .envir
#####################

#Yuncong(Oliver) Duan
# Professor Ziegler 
# 01/28/2020

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
setwd("~/Desktop/GitHub/QTM200Spring2020/problem_sets/PS3")

data<-read.csv("https://raw.githubusercontent.com/zieglerjef/QTM200Spring2020/master/problem_sets/PS3/incumbents_subset.csv")

summary(data)
names(data)
#####################
# Problem 1
#####################

#1
data<-read.csv("https://raw.githubusercontent.com/zieglerjef/QTM200Spring2020/master/problem_sets/PS3/incumbents_subset.csv")

linearMod1 <- lm(voteshare ~ difflog, data=data)
linearMod1


#2)
plot(data$difflog,data$voteshare,             
     xlab="Difflog",        
     ylab="Voteshare",
     main="Voteshare ~ Difflog",pch=10,col='blue',
     col.axis='red', xlim=c(0,6.66666), ylim=c(0.34,1.1)) 

abline(linearMod1, col="green")

#3)
residual1 <- resid(linearMod1)
plot(density(residual1))

#4)
#voteshare =  0.57903 + 0.04167*(difflog)

#####################
# Problem 2
#####################

#1)
linearMod2 <- lm(presvote ~ difflog, data=data)
linearMod2

#2)
plot(data$difflog,data$presvote,             
     xlab="Difflog",        
     ylab="Presvote",
     main="Presvote ~ Difflog",pch=6,col='blue',
     col.axis='red', xlim=c(0,6.66666), ylim=c(0.34,1.1)) 

abline(linearMod2, col="green")

#3)
residual2 <- resid(linearMod2)
plot(density(residual2))

#4)
#Presvote =  0.50758 + 0.02384*(difflog)

#####################
# Problem 3
#####################

#1)
linearMod3 <- lm(voteshare ~ presvote, data=data)
linearMod3

#2)
plot(data$presvote,data$voteshare,             
     xlab="Presvote",        
     ylab="Voteshare",
     main="Voteshare ~ Presvote",pch=8,col='blue',
     col.axis='red', xlim=c(0,1.4), ylim=c(0.34,1)) 

abline(linearMod3, col="red")

#3)
residual3 <- resid(linearMod3)
plot(density(residual3))

#4)
#Voteshare =  0.4413 + 0.3880*(presvote)

#####################
# Problem 4
#####################

#1)
linearMod4 <- lm(residual1 ~ residual2, data=data)
linearMod4
summary(linearMod4)

#2)
plot(residual2,residual1,             
     xlab="the variation in presvote is not explained by the difference in spending between incumbent and challenger in the district",        
     ylab="the variation in voteshare is not explained by the difference in spending between incumbent and challenger",
     main="Residual1 ~ Residual2",pch=16,col='blue',
     col.axis='red', xlim=c(-0.5,0.6), ylim=c(-0.4,0.5)) 

abline(linearMod4, col="red")


#3)
#Residual1 = -4.860e-18 + 2.569e-01*(Residual2)

#####################
# Problem 5
#####################

#1)
MultiLinear <- lm(voteshare ~ difflog + presvote, data=data)
MultiLinear
summary(MultiLinear)
confint(MultiLinear,level=0.96)
anova(MultiLinear)
#2)
#Voteshare = 0.44864 + 0.03554*(difflog) + 0.25688*(presvote)

#3)
# Analysing the relationship between residual1 and residual2, we find the probability(<2e-16) of the null hypothesis being true is extremely small(Ho:there is no relationship between residual1 and residual2) Thus, we can reject the null hypothesis and it is confirmed that there is a relationship between residual1 and residual2

# When analysing the relationship between voteshare and difflog + presvote, we find the probability (<2e-16) of the null hypothesis being true is extremely small(Ho:there is no relationship between voteshare and difflog + presvote). Thus, we can reject the null hypothesis and it is confirmed that there is a relationship between voteshare and difflog + presvote

#Residual1 records the unexplained variablity within the liear model built to explan voteshare ~ difflog

#Residual2 records the unexplained variability within the linear model built to explan presvote ~ difflog

#It has been proven that Residual1 could be modeled as a linear function by Residual2

#The coefficient of Residuel2 when trying to model Residual1  is identical the coefficient of presvote when we are trying to constrcut a relationship between difflog and voteshare in a multiple regression

#This is because during the actual construction of multiple regression, when we are trying to obtain the coefficient for each of the variable wrt the outcome, for example in this case when we are trying to find the relationship between difflog and voteshre in this multple regression, we are holding every other variable in the regression constant and only study how would this variable change wrt to the outcome. So in this case because we are doing voteshare ~ difflog, we are holding presvote constant. Due to the partial effect, the coefficient of presvote in this case is the "unexplained variablity" in the realtionship of voteshare ~ difflog. 


