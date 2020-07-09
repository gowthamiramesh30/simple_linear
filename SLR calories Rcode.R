library(readr)
calories<-read.csv(file.choose())
View(calories)
attach(calories)
library(moments)

#EDA

#1st business moment

mean(Weight.gained..grams.)
mean(Calories.Consumed)

median(Weight.gained..grams.)
median(Calories.Consumed)

mode(Weight.gained..grams.)
mode(Calories.Consumed)

#2nd business moment

var(Weight.gained..grams.)
var(Calories.Consumed)

sd(Weight.gained..grams.)
sd(Calories.Consumed)

range(Weight.gained..grams.)
range(Calories.Consumed)

boxplot(Weight.gained..grams.)
boxplot(Calories.Consumed)


#3rd business moment
skewness(Weight.gained..grams.)
skewness(Calories.Consumed)

#4th business moment
kurtosis(Weight.gained..grams.)
kurtosis(Calories.Consumed)

summary(calories)

#scatter plot
plot(Calories.Consumed,Weight.gained..grams.)

#correlation coefficient
cor(Calories.Consumed,Weight.gained..grams.)#linear,positive,strong

#simple linear regression
reg<-lm(Weight.gained..grams.~Calories.Consumed)

summary(reg)#R-squared=89.68%

pred<-predict(reg)

reg$residuals

mean(reg$residuals)
#RMSE
sqrt(mean(reg$residuals^2))

confint(reg,level = 0.95)
predict(reg,interval = "predict")

library(ggplot2)
ggplot(data = calories,aes(Weight.gained..grams.,Calories.Consumed))+
              geom_point(color='blue')+
              geom_line(color='red',data = calories,aes(Weight.gained..grams.,Calories.Consumed))

#logarthmic model

plot(log(Calories.Consumed),Weight.gained..grams.)

cor(log(Calories.Consumed),Weight.gained..grams.)#linear,positive,strong

reg_log<-lm(Weight.gained..grams.~log(Calories.Consumed))

summary(reg_log)#R-squared=80.77%

predict(reg_log)

reg_log$residuals

mean(reg_log$residuals)
sqrt(mean(reg_log$residuals^2))

confint(reg_log,lev=0.95)
predict(reg_log,interval = "confidence")


#exponential model
plot(Calories.Consumed,log(Weight.gained..grams.))


cor(Calories.Consumed,log(Weight.gained..grams.))

reg_expo<-lm(log(Weight.gained..grams.)~Calories.Consumed)

summary(reg_expo)#R-squared=87.76%

predict(reg_expo)
reg_expo$residuals

mean(reg_expo$residuals)
sqrt(mean(reg_expo$residuals^2))

logat<-predict(reg_expo)
at<-exp(logat)
error=calories$Calories.Consumed-at
error
sqrt(sum(error^2)/nrow(calories))


confint(reg_expo,level = 0.95)
predict(reg_expo,interval ="confidence" )

ggplot(data = calories,aes(x=Weight.gained..grams.,y=Calories.Consumed))+
       geom_point(color='blue')+
       geom_line(color='red',data = calories,aes(x=Weight.gained..grams.,y=Calories.Consumed))  

#quadratic equation
plot(Calories.Consumed,Weight.gained..grams.)
plot(Calories.Consumed*Calories.Consumed,Weight.gained..grams.)

cor(Calories.Consumed*Calories.Consumed,Weight.gained..grams.)

plot(Calories.Consumed*Calories.Consumed,log(Weight.gained..grams.))

cor(Calories.Consumed,log(Weight.gained..grams.))
cor(Calories.Consumed*Calories.Consumed,log(Weight.gained..grams.))

reg2degree<-lm(log(Weight.gained..grams.)~Calories.Consumed+I(Calories.Consumed*Calories.Consumed))

summary(reg2degree)#R-squared=87.76%

logpol <- predict(reg2degree)
expy <- exp(logpol)

err=calories$Weight.gained..grams.-expy

sqrt(sum(err^2)/nrow(calories))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

#r-squared value without any transformation is high that is 89.68% so without any transformation the model is good
