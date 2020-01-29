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
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS1")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
#Find the sum 
sum(y)
#Find the mean
sum(y)/length(y)
sample_mean=mean(y)
#Find the sum of errors
demeanedSumSimple = y - mean(y)
demeanedSumSimple
#Find the squared error
squaredError= demeanedSumSimple^2
squaredError
#Find the varience
varience=sum(squaredError)/length(y)
varience
#Find the standard deviation
Sd= sd(y,na.rm=FALSE)
sample_sd=Sd
#Given confidence coefficient = 0.9
z90=qt((1-0.9)/2,df=length(y)-1, lower.tail = FALSE)
n= length(y)
lower_90 = sample_mean-(z90*(sample_sd/sqrt(n)))
upper_90 = sample_mean+(z90*(sample_sd/sqrt(n)))
confint90 = c(lower_90,upper_90)
# [93.95993 102.92007]


#####################
# Problem 2
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# set Ho:mu < 100
# set Ha:mu >= 100

mu = 100
#Find the sum 
sum(y)
#Find the mean
mean(y)
#Find the sum of errors
demeanedSumSimple = y - mean(y)
demeanedSumSimple
sum(demeanedSumSimple)
#Find the squared error
squaredError= demeanedSumSimple^2
#Find the varience
varience=sum(squaredError)/length(y)-1
varience

z=(mean(y)-mu)/varience

critical_value = qt(0.05, df =length(y)-1, lower.tail = T )

if(critical_value<z){
   print("there is not enough evidence to disprove Ho, the null hypothesis, which states that students in her school is lower than the average IQ score 100 among all the schools in the country")
}
#There is not enough evidence to disprove Ho, the null hypothesis, which states that students in her school is lower than the average IQ score 100 among all the schools in the country, thus the hypothesis testing suggest the counselor's hypothesis might not be right 



#####################
# Problem 3
#####################
library(ggplot2)
y <- c(1, 2, 1, 3, 4, 1, 1, 4, 2, 1, 3, 4, 3, 2, 1, 3, 4, 1, 2, 3, 1, 1, 2, 1, 1, 3, 4)

expenditure <- read.table("expenditure.txt", header=T)

names(expenditure)
summary(expenditure)
hist(Region)
str(expenditure)

#1

#Y & X1
pdf("plot1.pdf")
ggplot(expenditure, aes(x=X1, y=Y)) + geom_point() + labs(x="per capital personal income", y="per capital expenditure on public education", title="")+geom_smooth()
dev.off()
#Observation: The relationship between X1 and Y is almost linear and the line is upwards slowping. With per capital personal income increases, per capital expenditure on public education also increases. 

#Y & X2
pdf("plot2.pdf")
ggplot(expenditure, aes(x=X2, y=Y)) + geom_point() + labs(x="number of residents per thousand under 18 years of age", y="per capital expenditure on public education", title="")+geom_smooth()
dev.off()
#Observation: The relationship between X2 and Y is almost linear and the line is downwards slowping. With number of residents per thousand under 18 years of age increases, per capital expenditure on public education also decreases.

#Y & X3
pdf("plot3.pdf")
ggplot(expenditure, aes(x=X3, y=Y)) + geom_point() + labs(x="number of people per thousand residing in the urban areas", y="per capital expenditure on public education", title="")+geom_smooth()
dev.off()
#Observation: The relationship between X3 and Y is not linear because the curve has 3 inflection points. When X3 approaches to 400, Y value is decreasing; when X3 approaches to 700, Y value is increasing; when X3 increases after 700, Y value decreases.

#2(graphics for this question is under #3 for no reason: I can't fix it :(  )
pdf("plot4.pdf")

input=expenditure[,c('Region','Y')]
print(head(input))
boxplot(Y~Region, data = expenditure, xlab = "Region",
ylab = "per capital expenditure on public education", main = "")

dev.off()

#Observation: I use the box plot to compare across regions in terms of per capital expenditure on public education. Accrording to the chart we find on average  West(4) region has the largest per capital expenditure on public education whereas  South(3) region has the lowest

#3

pdf("plot5.pdf")

ggplot(expenditure, aes(x=X1, y=Y))+ geom_point()+labs(x="per capital personal income", y="per capital expenditure on public education", title="")+ geom_smooth()
#Observation: The relationship between X1 and Y is almost linear and the line is upwards slowping. With per capital personal income increases, per capital expenditure on public education also increases.
dev.off()

pdf("plot6.pdf")
ggplot(expenditure, aes(x=X1, y=Y))+geom_point(aes(color=Region))+ labs(x="per capital personal income", y="per capital expenditure on public education", title="")
dev.off()



