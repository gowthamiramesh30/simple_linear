library(readr)
salary<-read.csv(file.choose())
View(salary)
attach(salary)

#EDA
summary(salary)

#scatter plot
plot(YearsExperience,Salary)

#correlation coefficient
cor(YearsExperience,Salary)

#simple linear regression
reg<-lm(Salary~YearsExperience)

summary(reg)#Rsquare=95.7%

predict(reg)

reg$residuals
mean(reg$residuals)

#RMSE
sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval = "predict")

#logarthmic model

plot(log(YearsExperience),Salary)

cor(log(YearsExperience),Salary)

reg_log<-lm(Salary~log(YearsExperience))

summary(reg_log)#Rsquare=85.39%

predict(reg_log)

reg_log$residuals

mean(reg_log$residuals)
sqrt(mean(reg_log$residuals^2))

confint(reg_log,lev=0.95)
predict(reg_log,interval = "confidence")

#exponential model

plot(YearsExperience,log(Salary))

cor(YearsExperience,log(Salary))

reg_expo<-lm(log(Salary)~YearsExperience)

summary(reg_expo)#Rsquare=93.2%

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

#without transformation the modle is good

