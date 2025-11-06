# Elizabeth Version of HW 10 #



######################################
#    Objective 1 
######################################

#A: Get Data and check normality 

#installing packages 
library(MASS) #for graphing stuff to checking normality 

#loading the data 

#I am going to bum Nathan's beautiful code htat takes it directly from the EDIS archive, in the meantime I just downloaded it manually 
ticks = read.csv("tick_rawdata.csv", header = TRUE)
d <- read.csv("name.csv", header = TRUE, row.names = 1)
#this says your header is a header for first row and first column is also a header 
#exploring the data
sum(ticks$wood.found, na.rm = TRUE)
sum(ticks$wood.bite, na.rm = TRUE)
sum(ticks$deer.found, na.rm = TRUE)
sum(ticks$deer.bite, na.rm = TRUE)

#Assumption 1 Linearity

#Scatterplot of hours spent and deer bites
plot(ticks$deer.bite ~ticks$hours) 
plot(jitter(ticks$deer.bite) ~ticks$hours)

plot(X,Y,ylim=c(0,100), xlim=c(0,10),cex=1.5,cex.lab=1.25,cex.axis=1.15)

#use function "lm: to plot a line through the data
reg<-lm(Y~X)
abline(reg, col="red", lwd=3) 

###CALCULATING A PLOTTING RESIDUALS

predicted<-predict.lm(reg)
residuals<-Y-predicted

##  plot residuals vs. the predicted values:
plot(predicted,residuals,cex=2,cex.lab=1.5,cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")


###Assumption 2 Homogeneity of Variances
#We look at the same plot of residuals vs. the predicted values

#Assumption 3 Normality
#calculate standardized residuals
stdRes = rstandard(reg)

#QQPLOT
qqnorm(stdRes,ylab="Standardized Residuals", xlab="Theoretical Quantiles")
qqline(stdRes, col=2,lwd=2)

#historgram of std residuals
hist(stdRes)