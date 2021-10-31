#Statistical Data Mining
#Assignment Homework
# UBIT : hsuvarna
# Created on :11 September
#Last updated : 15th September

################################
############Problem 1 and 2:
################################


#Installing the required packages 

#install.packages('stats')
#install.packages('dplyr')
#install.packages('DAAG')
#install.packages('lattice')
#install.packages('MASS')
#if (!requireNamespace("BiocManager", quietly = TRUE))
 # install.packages("BiocManager")

#BiocManager::install("geneplotter")


rm(list=ls())

#Loading the libraries
library(stats)
library(dplyr)
library(DAAG)
library(lattice)
library(MASS)
library(geneplotter)

#Loading given data from local system into variable mydata  
mydata <- read.csv("C:\\Users\\rishi\\Downloads\\cereal.csv")

names(mydata) #To check what all columns does the data have ?
dim(mydata) #To check the dimensionality 
str(mydata) 
summary(mydata) #To analyse the distribution and range of each column in the dataset

x11() #Plotting to check the relationship 
plot(mydata)

#After checking all the graphs between various variables, it visually tells us that rating has a dependency only on few variables such as fiber,potassium,sodium,fats and carbohydrates

plot(mydata$fiber,mydata$rating,xlab="Fiber",ylab="Rating",main="Scatterplot")
plot(mydata$potass,mydata$rating,,xlab="Potassium",ylab="Rating",main="Scatterplot")
plot(mydata$sodium,mydata$rating,xlab="Sodium",ylab="Rating",main="Scatterplot")
plot(mydata$fat,mydata$rating,xlab="Fats",ylab="Rating",main="Scatterplot")
plot(mydata$carbo,mydata$rating,xlab="Carbohydrates",ylab="Rating",main="Scatterplot")


#We could see sugars ,carbs and potass values for few rows are negative, so we can eliminate
##these rows as they cannot be true values

clear_data <- subset(mydata,mydata$sugars >=0 & mydata$potass >=0  & mydata$carbo >=0)
dim(clear_data) # Removed 3 rows with negative values

plot(clear_data)
names(clear_data)
boxplot(clear_data$fiber,main='Fiber')
summary(clear_data)


clean1 <- clear_data  #We shall call clean1, clean2 etc as we go on cleaning the dataset 
summary(clean1)
boxplot(clean1$calories)



# To remove outliers , we are setting up an upper fence on the boxplot, which we are considering to be 
## equal to Upper_fence= 3rd Qu +1.5 times the interquartile range ( Q3-Q1)
###We shall proceed to calculate the upper fence for all the features.


calories_upfen <- 110+1.5*10  #Calculating the upper fence with the values obtained from the summary ( Q1,Q3)

summary(clean1$fat)
#summary(clean1$protein)
protein_upfen <- 3+1.5*1
fat_upfen <- summary(clean1$fat)[5] + 1.5*(summary(clean1$fat)[5]-summary(clean1$fat)[2])
sodium_upfen <- summary(clean1$sodium)[5] + 1.5*(summary(clean1$sodium)[5]-summary(clean1$sodium)[2])
fiber_upfen <- summary(clean1$fiber)[5] + 1.5*(summary(clean1$fiber)[5]-summary(clean1$fiber)[2])
carbo_upfen <- summary(clean1$carbo)[5] + 1.5*(summary(clean1$carbo)[5]-summary(clean1$carbo)[2])
sugars_upfen <- summary(clean1$sugars)[5] + 1.5*(summary(clean1$sugars)[5]-summary(clean1$sugars)[2])
potass_upfen <- summary(clean1$potass)[5] + 1.5*(summary(clean1$potass)[5]-summary(clean1$potass)[2])
vitamins_upfen <- summary(clean1$vitamins)[5] + 1.5*(summary(clean1$vitamins)[5]-summary(clean1$vitamins)[2])
weight_upfen <- summary(clean1$weight)[5] + 1.5*(summary(clean1$weight)[5]-summary(clean1$weight)[2])
cups_upfen <- summary(clean1$cups)[5] + 1.5*(summary(clean1$cups)[5]-summary(clean1$cups)[2])

#Once we have the boundaries for all the features, we proceed to eliminate all these outliers using the subset function
clean2 =subset(clean1,calories<=calories_upfen & protein<=protein_upfen & fat<fat_upfen & sodium<=sodium_upfen & fiber<=fiber_upfen & 
                 carbo<=carbo_upfen & sugars<=sugars_upfen & potass<=potass_upfen & vitamins<=vitamins_upfen & 
                 weight<=weight_upfen & cups<=cups_upfen)

#We can actually also ignore the column "name" as the rating would not be affected by the name of the cereal
str(clean2)
clean2 <- clean2[,!(names(mydata) %in% c("name"))]
clean2 <- clean2[,-1] # Omitting the row "mfr" as it would have no dependency on rating

str(clean2)
#Since we dropped all rows with outliers, we would have a reduced dimensionality 
# Clean2 will be my dataset after all the data cleaning is done
head(clean2)

#Saving this cleaned dataset as an RData file
getwd()
save(clean2,file="Clean2.RData")



mul_reg <- lm(rating~calories+protein+fat+sodium+fiber+carbo+sugars+potass+vitamins+shelf+weight+cups,data=clean2)
summary(mul_reg)

######Answer for 2(a)

#Looking at the summary of mul_reg, we can say that all the variables except shelf,weight and cups have a good significance to calculate the rating of the cereal
##This can be inferred from the fact that the coefficients of the variables are all around -3.4e-02 until 5.493e+01. 
##Also the summary of the linear model has the significant * which tells us how significant a predictor is in calculating the response 


##Answer for 2(b)

#The coefficient variable for sugar(=-7.249e-01) says that , in the final equation to calculate the rating, the coefficient against the variable sugars would be (-7.249*0.1), i.e -0.7249
##Though it looks like the value of the coefficient is small, it is equally significant when comparing to most of the predictor coeffs.

##Answer for 2(c)
 
mul_reg_inter <- lm(rating~(calories+protein+fat+sodium+fiber+carbo+sugars+potass+vitamins+shelf+weight+cups)^2,data=clean2)   
summary(mul_reg_inter)

#When checking for interactions with all the variables, the coefficients are mostly in the order of e-05 and lower, which tells us that the interactions are quite insignificant 
