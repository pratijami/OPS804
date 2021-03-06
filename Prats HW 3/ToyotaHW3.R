getwd()
set.seed(1)

#libraries
library(lpSolveAPI)
library(lpSolve)
library(MASS)
library(psych)
library(plyr)
library(gridExtra)
library(leaps)
library(lmtest)
library(sandwich)
library(e1071)
library(broom)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(foreign)
library(Formula)
library(reshape)
library(pastecs)
library(dplyr) # for data preparation
library(car) # for levenes test
library(knitr)

#Read CSV
Toyotacor <- read.csv("ToyotaCorolla.csv")
stringsAsFactors=FALSE
ls(Toyotacor)
Toyotacor


#Price - The price of the car 
#Age - The age of the car in month 
#KM - Accumulated kilometer on the odometer 
#FuelType - Type of fuel used 
#HP - Horsepower of the car 
#MetColor - Whether the car has metallic (1) or not (0) 
#Transmission - Whether the car has automatic transmission (1) or not (0) 
#CC - Cylinder volumen in cubic centimeter 
#Doors - Number of doors 
#Weight - Weight of the car in kilogram 
head(Toyotacor)
plot(Price ~ KM, data = Toyotacor)
plot(Price ~ Age, data = Toyotacor)
plot(Price ~ FuelType, data = Toyotacor)
plot(Price ~ HP, data = Toyotacor)
plot(Price ~ MetColor, data = Toyotacor)
plot(Price ~ Automatic, data = Toyotacor)
plot(Price ~ CC, data = Toyotacor)
plot(Price ~ Doors, data = Toyotacor)
plot(Price ~ Weight, data = Toyotacor)


#Toyotacor.sum <- describe(dcast(Toyotacor, id ~ FuelType, value.var = "Price")[,-1])
#Toyotacor.sum

#1.Descriptive Statistics

DS <- stat.desc(Toyotacor)
print(DS)

qqnorm(Toyotacor$Price)
qqline(Toyotacor$Price)
ggsave("HW3 Graph/ToyotaCorQQ.pdf")

#Regression Analysis - all data #2
Toyotacor.m1 <- lm(Price ~ ., data = Toyotacor)
Toyotacor.m1.summary <- summary(Toyotacor.m1)
Toyotacor.m1.summary
get.objective(Toyotacor.m1.summary)
plot(Price ~ ., data = Toyotacor.m1)
#3. 
#predicted Values/ Residuals
Toyotaco_hat <- fitted(Toyotacor.m1)# predicted values
as.data.frame(Toyotaco_hat)
Toyota_resid <- residuals(Toyotacor.m1) # residuals
options(max.print = 10)
as.data.frame(Toyota_resid)

#Confident Interval
Toyotacor.m1.confint <- confint(Toyotacor.m1)
Toyotacor.m1.confint

#checking corelation matrix
library(ISLR)
library(plm)
sapply(Toyotacor,class)
#corelation matrix
Toyotacor.D= as.data.frame(matrix(Toyotacor))[1,]
print(Toyotacor.D)
TyCo.col <- cor(data.frame(lapply(Toyotacor.D, rank)))
TyCo.col
layout(matrix(1:4,2,2))
plot(TyCo.col)
#calculate regression - Model 1
options(max.print = 10)
x <- Toyotacor[, 2:10] # independent variable
x
y <- Toyotacor[,1] # dependent variable
y
#model selection 
options(max.print = 10)
Toyotacor.out <- summary(regsubsets(x,y, nbest = 2, nvmax = ncol(x)))
Toyotacor.regtab <- cbind(Toyotacor.out$which, Toyotacor.out$rsq, Toyotacor.out$adjr2, Toyotacor.out$cp)

colnames(Toyotacor.regtab) <- c("(Intercept)", "Age", "KM", "FuelType", "HP", "MetColor", "Transmission", "CC" , "Doors", "Weight", "R-Sq", "R-Sq(adj)", "Cp")
Toyotacor.regtab
#pvalue is < 0.05, so we reject the null hypothesis

#create second model 
options(max.print = 10)
Toyotacor.m2 <- lm(Price~ Age + FuelType+HP+MetColor+ Automatic+ CC + Doors, data = Toyotacor) #just with weight
Toyotacor.m2.summary <- summary(Toyotacor.m2)
print(Toyotacor.m2.summary)


#Model 2 Conf Intervals
options(max.print = 10)
Toyotacor.m2.confint <- confint(Toyotacor.m2)
print(Toyotacor.m2.confint)

Toyotacor.m2 <- lm(Price~ Age + FuelType+HP+MetColor+ Automatic+ CC + Doors, data = Toyotacor)
Toyotacor.m2

#Assigning values
options(max.print = 10)
given.Toyotacor <- data.frame(Age=12, FuelType=1, HP=185, MetColor=1, Automatic=0, CC=2000, Doors=4)
predicted.price <- predict(Toyotacor.m2,given.Toyotacor)
print(predicted.price)
# Create a loop to test 
options(max.print = 10)

n <- length(Toyotacor$Price)
diff <- dim(n)
percdiff <- dim(n)
for (k in 1:n) {
  train1 <- c(1:n)
  train <- train1[train1 !=k ]
  m2 <- lm(Price~ ., data = Toyotacor[train,])
  pred <- predict(m2, newdat = Toyotacor[-train,])
  obs <- Toyotacor$Price[-train]
  diff[k] <- obs - pred
  percdiff[k] <- abs(diff[k]) / obs
}
Toyotacor.m2.me <- mean(diff)
Toyotacor.m2.rmse <- sqrt(mean(diff**2))
Toyotacor.m2.mape <- 100*(mean(percdiff))

Toyotacor.m2.me
Toyotacor.m2.rmse
Toyotacor.m2.mape
predicted.price



#check if assumptions are met...
plot(Toyotacor.m1)
ggsave("HW3 Graph/ToyotaCorlin.pdf")

