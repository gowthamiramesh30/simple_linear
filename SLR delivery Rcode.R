library(readr)
delivery<-read.csv(file.choose())
attach(delivery)
View(delivery)

#EDA
summary(delivery)

#scatter plot
plot(Sorting.Time,Delivery.Time)

#correlation coefficient
cor(Sorting.Time,Delivery.Time)

#simple linear regression
reg<-lm(Delivery.Time~Sorting.Time)

summary(reg)#Rsquare=68.23%

predict(reg)

reg$residuals
mean(reg$residuals)
#RMSE
sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval = "predict")


#logarthmic model

plot(Sorting.Time,log(Delivery.Time))

cor(Sorting.Time,log(Delivery.Time))

reg_log<-lm(log(Delivery.Time)~Sorting.Time)

summary(reg_log)#Rsquare=71.09%

predict(reg_log)

reg_log$residuals

mean(reg_log$residuals)
sqrt(mean(reg_log$residuals^2))

confint(reg_log,lev=0.95)
predict(reg_log,interval = "confidence")

#exponential model

plot(Sorting.Time,log(Delivery.Time))

cor(Sorting.Time,log(Delivery.Time))

reg_expo<-lm(log(Delivery.Time)~Sorting.Time)

summary(reg_expo)#Rsquare=71.09%
predict(reg_expo)
reg_expo$residuals

mean(reg_expo$residuals)
sqrt(mean(reg_expo$residuals^2))

logat<-predict(reg_expo)
at<-exp(logat)

error=delivery$Delivery.Time-at
error
sqrt(sum(error^2)/nrow(delivery))

confint(reg_expo,level = 0.95)
predict(reg_expo,interval ="confidence" )

#quadratic equation
plot(Sorting.Time,Delivery.Time)
plot(Sorting.Time*Sorting.Time,Delivery.Time)

cor(Sorting.Time*Sorting.Time,Delivery.Time)

plot(Sorting.Time*Sorting.Time,log(Delivery.Time))

cor(Sorting.Time,log(Delivery.Time))
cor(Sorting.Time*Sorting.Time,log(Delivery.Time))

reg2degree<-lm(log(Delivery.Time)~Sorting.Time+I(Sorting.Time*Sorting.Time))

summary(reg2degree)#Rsquare=76.49%

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err=delivery$Delivery.Time-expy
sqrt(sum(err^2)/nrow(delivery))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

#polynomial with 3degree

reg3degree<-lm(log(Delivery.Time)~Sorting.Time+I(Sorting.Time*Sorting.Time)+I(Sorting.Time*Sorting.Time*Sorting.Time))

summary(reg3degree)#rsquare=78.19%

logpol3<-predict(reg3degree)
expy3<-exp(logpol3)

#polynomial with 3 degree is the best model


     

