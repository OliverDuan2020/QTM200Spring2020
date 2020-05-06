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

# set working directory
getwd()
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS7")

install.packages("lme4")
library("Matrix")
library("lme4")
library("ggplot2")
install.packages("googleVis")
library("googleVis")
install.packages("sjPlot")
library("sjPlot")

lapply(c("sjPlot", "googleVis"),  pkgTest)

#####################
# Problem 1
#####################

#Import data
mexico_elections_result <- read.csv("MexicoMuniData.csv", stringsAsFactors = F, header=T)
#a) 
model_1 <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, family = "poisson", data=mexico_elections)
model_1
anova(model_1, test = "Chisq")
##Since the p-value for competitive district is greater than 0.05, it is very hard to reject the null hypothesis 
##There is not enough evidence to suggest that whether the district is highly contested or not has a statistically significant effect on the number of times PAN presideential candidates visited.

#b)
#The coefficient for marginality.06 is -2.098 means that the expected log count for a one-unit increase in marginality.06 is -2.098.
#The district being a safe seat would decrease the log odds of marginality.06 by 2.098.
#The coefficient for PAN.governor.06 is -0.207 is referring to the fact that keeping other variables constant, the expected log count for a one-unit increase in PAN.governor.06 is -0.207

#c)
Y = -3.9304 - 0.4594*competitive.district - 2.0981* marginality.06 - 0.2073*PAN.governor.06  
#When competitive.district = 1, marginality.06 = 0, and PAN.governor.06 = 1,
#Y would be -4.60.

#####################
# Problem 2
#####################

#1)
pooled_1 <- lm(Reaction ~ Days, data=lme4)
#2)
unpooled_2 <- lm(Subject, data=lme4)
#3)
unpooled_3 <- lm(Reaction ~ Subject:Days, data=lme4)
#4)
unpooled_4 <- lm(Reaction ~ Subject + Days + Subject:Days, data=lme4)
#5) 

