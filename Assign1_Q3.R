#Statistical Data Mining
#Assignment Homework
# Created on :13 September

################################
############Problem 3:
################################


rm(list=ls())

#Installing corrplot
#install.packages('corrplot')
library(corrplot)
library(MASS)
mydata <- MASS::Boston
head(mydata)
?Boston
dim(Boston)
#For making pairwise scatter plots

##Answer for (a)
x11()
pairs(Boston)
x <- cor(Boston)
corrplot(x)

#observations

#Getting the correlations between all the variables using the corrplt() function :
x <- cor(Boston)
corrplot(x)

#From the corrplot , we can observe that medv is highly dependent on the vaiables lstat, rm,ptratio
#From the plots against medv as the response variable and all other predictors on the X axis,the below observations are made :

plot(mydata$lstat,mydata$medv,xlab='% of low status population',ylab='Med of owner occupied homes')
##Observation 1) As the lower status of the population reside in a suburb increase, the medv of owner occupied homes tend to decrease ( Inverse relation) 

plot(mydata$rm,mydata$medv,xlab='Rooms per dwelling',ylab='Med of owner occupied homes')
###Observation 2) As rooms per dwelling increase, the owners tend to occupy them( Positive trend)
x11()
plot(mydata$ptratio,mydata$medv,xlab='Pupil to teacher ratio',ylab='Med of owner occupied homes')
##Observation 3 : There is no evident relqation between the ptratio and the medv 


###Answer for (b)
#After plotting all the pairwise plots, we can say that per capita crime rate is dependent on   lstat
x11()
pairs(Boston)
plot(Boston$lstat,Boston$crim)   
#Observation# As the lower status population percentage in the area increases, the crime rate tends to increase 




###Answer for (c):
x11()
boxplot(Boston$crim,main="Boston per capita crime rate in suburbs")
summary(Boston)
#Observation: 1)Though the median of per capita crime rate is around 0.2, the mean value is close to 3.6 which is close the 3rd quartile.
##This explains that any suburb with a per capita crime rate of more than 0.25 can be considered as a suburb with seriously high crime rate.
##2) The tax rates are spread from a minimum of 187 until a maximum of 711. What is interesting is that the median and mean are not seperated by a large marigin 
#### but the values Median-Q1 is much lesser than  (Q3-Median) ,which means that a majority of suburbs do have tax rates which are high.
##3) pt ratio values are uniformly spread from a range of 12.60 to 22.0, with a mean value of 18 which is >> (min+max)/2 which indicates that a large number
###of observations are on the higher side of the mean.

###Answer for (d):
mydata <- Boston
dim(mydata)
median(mydata$medv)
#We have a total suburb count of 64
str(mydata)
bedrooms <- subset(mydata,mydata$rm>=7)
dim(bedrooms)
#Total no of suburbs with average rooms per dwelling greater than 7 are of count 64

bedrooms <- subset(mydata,mydata$rm>=8)
dim(bedrooms)
##We also have 13 suburbs with mean bedrooms per dwelling greater than 8
str(bedrooms)
median(bedrooms$medv)
