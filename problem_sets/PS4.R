#####################
# load libraries
# set wd
# clear global .envir
#####################

#Yuncong(Oliver) Duan
# Professor Ziegler 
# 02/18/2020

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

getwd()
setwd("~/Desktop/GitHub/QTM200Spring2020/problem_sets/PS4")

#####################
# Problem 1
#####################

install.packages(car)
library(car)
data(Prestige)
help(Prestige)

summary(Prestige)

#a)
professional<-ifelse(Prestige$type=="prof",1,0)

#b)
LinearModel1=lm(prestige ~ income + professional+ income*professional, data = Prestige)
LinearModel1
summary(LinearModel1)

#c)
#Prestige =  21.1422589 + 0.0031709*(income)+ 37.7812800*(professional) -0.0023257*( income*professional)

#d)
#The coefficient for income is a positive number, which suggests that income is positively correlated with prestige. Moreover, the coefficient suggests that when there is a dollar increase in income, there will also be 0.0031709 unit of increase in the value of prestige, given all other factors in the model are constant.

#e)
#The coefficient for professional is a positive number, which suggests that professional is positively correlated with prestige. Moreover, the coefficient suggests that when this person is identified as professional, his prestige value will immediately increase by 37.78 units,given all other factors in the model are constant.

#f)
# Prestige =  21.1422589 + 0.0031709*(income)+ 37.7812800*(professional) -0.0023257*( income*professional)
#Because we are talking about professional only
# professional =1
# Prestige = (21.1422589+37.7812800)+(0.0031709-0.0023257)*(income)=59.92 + 0.0008*income
#When income increases by $1000 
# difference = 0.0008*1000 = 0.8
# The effect of $1000 increase in the income given professional is 1 will be a 0.8 unit of value increases for prestige.

#g)
# Prestige =  21.1422589 + 0.0031709*(income)+ 37.7812800*(professional) -0.0023257*( income*professional)
#When income = $6000, and professional =0 
# Prestige = 21.1422589 + 0.0031709*(6000)+ 37.7812800*(0) -0.0023257*( 6000*0)
# Prestige = 40.168
#When income = $6000, but professional =1
# Prestige = 21.1422589 + 0.0031709*(6000)+ 37.7812800*(1) -0.0023257*( 6000*1)
# Prestige = 63.994
#Difference = 63.994-40.168 = 23.826
#Even though the personal's income is not changed, when he switches from being a non-professional to be a professional, the value of prestige increases by 23.826 units

#####################
# Problem 2
#####################

# Set the coefficient in front of "Precinct assigned lawn signs" as beta1 and the coefficient in front of "Precinct adjacent to lawn signs" as beta2

#vote = 0.302 + 0.042*(assigned) + 0.042* (adjacent)

#a)
# beta1 =0.042;se(beta1) = 0.016
#Hypothesis:
# Ho: beta1 = 0
# Ha: beta 1 ≠ 0

#Test_Statistics=(0.042-0)/0.016 = 2.625 
#df =131-3 = 128
p_value = 2*pt(2.626,128,lower.tail = F)
#P_value = 0.0096
# Because P_value is smaller than 0.05, there is enough evidence to reject the Ho, so having these yard signs in a precinct affects vote share

#b)
# beta2 = 0.042; se(beta2) = 0.013
#Hypothesis:
# Ho: beta2 = 0
# Ha: beta2 ≠ 0

#Test_Statistics=(0.042-0)/0.013 = 3.23 
#df =131-3 = 128
p_value = 2*pt(3.23,128,lower.tail = F)
#P_value = 0.0015
# Because P_value is smaller than 0.05, there is enough evidence to reject the Ho, so being next to precincts with these yard signs affects vote share

#c)
#constant term = 0.302
#Interpretation: Regardless of whether having these yard signs in a precinct or being next to precincts with these yard signs, there is always going to be 3.02% of the vote that went to McAuliff’s opponent Ken Cuccinelli.

#d)
# The strength of the fit of a linear model is mostly evaluated using R_Square, in this model, the value of R_Square is only 0.094, which suggest that only 9.4% of the variability in the proportion of the vote share that went to McAuliff’s opponent Ken Cuccinelli is explained by factors related to the yard signs.Thus, other vectors that are not included in the model must have more influence on the proportion of vote share than the yard signs. In another word, comparing with other vectors, yard sign's effect on the outcome variable is rather trivial.
