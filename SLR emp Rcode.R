library(readr)
emp<-read.csv(file.choose())
View(emp)
attach(emp)

#EDA
summary(emp)

#scatter plot
plot(Salary_hike,Churn_out_rate)

#correlation coefficient
cor(Salary_hike,Churn_out_rate)#linear,negative,strong

#simple linear regression
reg<-lm(Churn_out_rate~Salary_hike)

summary(reg)#Rsquare=83.12%

predict(reg)

reg$residuals
mean(reg$residuals)
#RMSE
sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval = "predict")

#logarthmic model

plot(log(Salary_hike),Churn_out_rate)

cor(log(Salary_hike),Churn_out_rate)

reg_log<-lm(Churn_out_rate~log(Salary_hike))

summary(reg_log)#Rsquare=84.86%

predict(reg_log)

reg_log$residuals

mean(reg_log$residuals)
sqrt(mean(reg_log$residuals^2))

confint(reg_log,lev=0.95)
predict(reg_log,interval = "confidence")

#exponential model

plot(Salary_hike,log(Churn_out_rate))

cor(Salary_hike,log(Churn_out_rate))

reg_expo<-lm(log(Churn_out_rate)~Salary_hike)

summary(reg_expo)#Rsquare=87.35%

predict(reg_expo)
reg_expo$residuals

mean(reg_expo$residuals)
sqrt(mean(reg_expo$residuals^2))

logat<-predict(reg_expo)
at<-exp(logat)

error=emp$Churn_out_rate-at
error

sqrt(sum(error^2)/nrow(emp))

confint(reg_expo,level = 0.95)
predict(reg_expo,interval ="confidence" )

#exponential modle is the best






