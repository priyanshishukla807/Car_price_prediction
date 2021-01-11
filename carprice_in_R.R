library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)
install.packages("boot")
install.packages("car")
install.packages("QuantPsyc")
install.packages("lmtest")
install.packages("sandwich")
install.packages("vars")
install.packages("nortest")
install.packages("MASS")
install.packages("caTools")
install.packages("dplyr")
setwd("D:\\vidhi")
getwd()
data = read.csv("CarPrice_Assignment.csv")
data1 = data
str(data1)
head(data1)
summary(data1)
dim(data1)
View(data1)
#Outlier Treatment through quantile method
quantile(data1$price,c(0,0.01,0.5,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,1))
#there is outliers in data1
boxplot(data1$price)
data2 = data1[data1$price < 32472,]
nrow(data1)
nrow(data2)
nrow(data1)-nrow(data2)
quantile(data2$price,c(0.01,0.5,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,1))
boxplot(data2$price)
data3 = data2[data2$price <25000,]
nrow(data2)
nrow(data3)
quantile(data3$price,c(0.01,0.5,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,1))
boxplot(data3$price)
##There is no outliers in data3
data.frame(colSums(is.na(data3)))
sapply(data3, function(x) sum(is.na(x)))
#there is no any na values in data
data3$cylindernumber = as.numeric(data3$cylindernumber)
View(data3)
# data has 205 rows and 26 columns. Car_ID is a unique identifier for each car, so we can ignore it. Our target variable is the price, which signifies the price of the said car. We will use other variable except the CarName.

set.seed(2)
train<- sample(1:nrow(data3), nrow(data3)*0.75)
test<- -train
train <- data3[train,]
test <- data3[test,]
nrow(train)
nrow(test)
fit = lm(price~car_ID+symboling+CarName+fueltype+aspiration+doornumber+carbody+drivewheel+enginelocation+wheelbase+carlength+carwidth+
           carheight+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
           highwaympg,data = train)
#fit is showing error - "Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : contrasts can be applied only to factors with 2 or more levels"
"This error means there is some columns in data with 2 or less levels ,So these columns are not required for fitting the model "
# we will remove these columns"
(l <- sapply(train, function(x) is.factor(x)))
m <- train[, l]
ifelse(n <- sapply(m, function(x) length(levels(x))) <= 2, "DROP", "NODROP") ## This will categorise all the factor variables under Drop and Nodrop category. Those falling under Drop, we do not have to take those variables in our model
# we will not consider the columns which have 2 or less levels.
fit = lm(price~symboling+carbody+drivewheel+wheelbase+carlength+carwidth+
           carheight+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
           highwaympg,data = train)
summary(fit)
fit = lm(price~symboling+carbody+wheelbase+carlength+carwidth+
           carheight+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
           highwaympg,data = train)
summary(fit)
fit = lm(price~symboling+carbody+wheelbase+carwidth+
           carheight+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
           highwaympg,data = train)
summary(fit)
fit = lm(price~symboling+carbody+carwidth+
           carheight+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
           highwaympg,data = train)
summary(fit)
fit = lm(price~symboling+carbody+carwidth+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
           highwaympg,data = train)
summary(fit)
fit = lm(price~symboling+carbody+carwidth+curbweight+enginetype+cylindernumber+fuelsystem+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
           highwaympg,data = train)
summary(fit)
fit = lm(price~symboling+carbody+carwidth+curbweight+enginetype+cylindernumber+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
           highwaympg,data = train)
summary(fit)
fit = lm(price~carbody+carwidth+curbweight+enginetype+cylindernumber+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
           highwaympg,data = train)
summary(fit)
fit = lm(price~carbody+carwidth+curbweight+enginetype+cylindernumber+stroke+compressionratio+horsepower+peakrpm+citympg+
           highwaympg,data = train)
summary(fit)
fit = lm(price~carbody+carwidth+curbweight+enginetype+cylindernumber+stroke+compressionratio+horsepower+citympg+
           highwaympg,data = train)
summary(fit)
fit = lm(price~carbody+carwidth+curbweight+enginetype+cylindernumber+stroke+compressionratio+horsepower+
           highwaympg,data = train)
summary(fit)
fit = lm(price~carbody+carwidth+curbweight+enginetype+cylindernumber+stroke+compressionratio+horsepower,data = train)
summary(fit)
fit = lm(price~carbody+carwidth+curbweight+enginetype+cylindernumber+stroke+horsepower,data = train)
summary(fit)
fit = lm(price~carbody+carwidth+curbweight+I(enginetype == "ohc")+I(enginetype == "rotor")+stroke+horsepower,data = train)
summary(fit)
fit = lm(price~I(carbody =="hardtop")+I(carbody =="hatchback")+I(carbody =="wagon")+carwidth+curbweight+I(enginetype == "ohc")+I(enginetype == "rotor")+stroke+horsepower,data = train)
summary(fit)
#Checking vif
vif(fit)
fit = lm(price~I(carbody =="hardtop")+I(carbody =="hatchback")+I(carbody =="wagon")+I(enginetype == "ohc")+I(enginetype == "rotor")+stroke,data = train)
summary(fit)
fit = lm(price~I(carbody =="hardtop")+I(carbody =="hatchback")+I(enginetype == "ohc")+I(enginetype == "rotor")+stroke+horsepower,data = train)
summary(fit)
fit = lm(price~I(carbody =="hardtop")+I(carbody =="hatchback")+I(enginetype == "ohc")+stroke+horsepower,data = train)
summary(fit)
vif(fit)
fitted(fit)
par(mfrow=c(2,2))
plot(fit)

#Calculating mape
train$pred <- predict(fit, train)
attach(train)
(sum((abs(price-pred))/price))/nrow(train)
##Value of Mape is 0.1804 so the fitted model is 1-0.1804 = 0.819 or 81.9% accurate
##Testing autocorelation
dwtest(fit)
#Since, the p-value is >0.05, we fail to reject H0: (No Autocorrelation) 
dwt(fit)
#Testing homoscadisticity
bptest(fit)
#p-value is les than 0.05 so the null hypothesis will reject ,error is homogenious.
#Normality test:-
resids<-fit$residuals
ad.test(resids)
write.csv(train,"Final.csv")
#p-value is less than 0.05 so data is not normally distributed
#################Validation on test data###########################################################################
fit_t = lm(price~symboling+carbody+drivewheel+wheelbase+carlength+carwidth+
             carheight+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
             highwaympg,data = test)
summary(fit_t)
fit_t = lm(price~carbody+drivewheel+wheelbase+carlength+carwidth+
             carheight+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
             highwaympg,data = test)
summary(fit_t)
fit_t = lm(price~carbody+wheelbase+carlength+carwidth+
             carheight+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
             highwaympg,data = test)
summary(fit_t)
fit_t = lm(price~carbody+wheelbase+carwidth+
             carheight+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
             highwaympg,data = test)
summary(fit_t)
fit_t = lm(price~carbody+wheelbase+
             carheight+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
             highwaympg,data = test)
summary(fit_t)
fit_t = lm(price~carbody+wheelbase+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
             highwaympg,data = test)
summary(fit_t)
fit_t = lm(price~carbody+wheelbase+enginetype+cylindernumber+enginesize+fuelsystem+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+
             highwaympg,data = test)
summary(fit_t)
fit_t = lm(price~carbody+wheelbase+enginetype+cylindernumber+fuelsystem+boreratio+stroke+compressionratio+peakrpm+citympg+
             highwaympg,data = test)
summary(fit_t)
fit_t = lm(price~carbody+wheelbase+enginetype+cylindernumber+I(fuelsystem == "midi")+I(fuelsystem == "spdi") +boreratio+stroke+compressionratio+peakrpm+citympg+
             highwaympg,data = test)
summary(fit_t)
fit_t = lm(price~carbody+wheelbase+enginetype+cylindernumber +boreratio+stroke+compressionratio+peakrpm+citympg+
             highwaympg,data = test)
summary(fit_t)
fit_t = lm(price~carbody+wheelbase+cylindernumber +boreratio+stroke+compressionratio+peakrpm+citympg+
             highwaympg,data = test)
summary(fit_t)
fit_t = lm(price~carbody+wheelbase+cylindernumber +boreratio+stroke+peakrpm+citympg+
             highwaympg,data = test)
summary(fit_t)
fit_t = lm(price~carbody+wheelbase+cylindernumber +boreratio+stroke+peakrpm+citympg,data = test)
summary(fit_t)
fit_t = lm(price~carbody+wheelbase+cylindernumber +boreratio+stroke+citympg,data = test)
summary(fit_t)
fit_t = lm(price~carbody+wheelbase+cylindernumber +stroke+citympg,data = test)
summary(fit_t)
fit_t = lm(price~carbody+wheelbase+cylindernumber +citympg,data = test)
summary(fit_t)
#Checking vif
vif(fit_t)
#get the predicted or fitte values
fitted(fit_t)
par(mfrow=c(2,2))
plot(fit_t)
#Calculating mape
test$pred <- predict(fit_t, test)
attach(test)
(sum((abs(price-pred))/price))/nrow(test)
##Value of Mape is 0.15149 so the fitted model on test data is 1-0.15149 = 0.8485 or 84% accurate
##Testing autocorelation
dwtest(fit_t)
#there is no autocorrelation
dwt(fit_t)
#checking homoscadisticity
bptest(fit_t)
#p-value is greater than 0.05 so the null hypothesis will not reject there is no homoscadisticity.
#Normality test:-
resids<-fit_t$residuals
ad.test(resids)
#p-value is less than 0.05 hence the data is not normally distributed