# Elizabeth Version of HW 10 #



######################################
#    Objective 1 
######################################

#A: Get Data and check normality 
#--------------------------------------------------------
#installing packages 
library(readxl) #so we can read the excel files 
library(janitor) #so we can convert to snakecase

#Getting data ready 
d = read_excel("Copy_of_bee_data.xlsx")
d = clean_names(d) #convert to snake case 
d$total_distance_m = as.numeric(d$total_distance_m) #convert to numeric
d = d[,c(8,28)] #pull out the columns we want 

#C 
##--------------------------------------------------------

#Checking normality 
hist(d$weight_out_of_nest_g) #beautiful and normal 
hist(d$total_distance_m, breaks = 100) #revolting! 

y = log(d$total_distance_m + 1) #not great but not as bad 
hist(y)

d$log_distance = log(d$total_distance_m + 1)

#i think it's actually quadratic 

#here's a model for a quadratic relatoinship
mod <-lm(d$total_distance_m ~ d$weight_out_of_nest_g + I(d$weight_out_of_nest_g^2), data = d)


#Assumption 1 Linearity

#Scatterplot of bee weight vs. distance 
plot(d$log_distance~d$weight_out_of_nest_g)

#B fitting a linear regression 
#--------------------------------------------------------------
reg = lm(d$log_distance~d$weight_out_of_nest_g) #forcing it into linear, log midified 
#mod <-lm(d$total_distance_m ~ d$weight_out_of_nest_g + I(d$weight_out_of_nest_g^2), data = d) #if we assume quaadratic 
abline(reg, col = 'red', lwd = 3)


#C still checking if it's linear  
#------------------------------------------------------------
predicted = predict.lm(reg) 
residuals = d$log_distance[1:331] - predicted

##  plot residuals vs. the predicted values:
plot(predicted,residuals,cex=2,cex.lab=1.5,cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")
#not great 

###Assumption 2 Homogeneity of Variances
#We look at the same plot of residuals vs. the predicted values

#Assumption 3 Normality
#calculate standardized residuals
stdRes = rstandard(reg)

#QQPLOT
qqnorm(stdRes,ylab="Standardized Residuals", xlab="Theoretical Quantiles")
qqline(stdRes, col=2,lwd=2) #not amazing but not terrible 

#historgram of std residuals
hist(stdRes)

#OK it's actually okay good as a log transform 

#D Making predictions 
#------------------------------------------------------------------------------

#how far does our model think bees of a median weight, and super fat bees at the 95th percentile, will fly? 

median = median(d$weight_out_of_nest_g)
x_95 = quantile(d$weight_out_of_nest_g, 0.95)

reg$coefficients
 # y = mx + b 
 # distance = 21.033(weight out of nest) + -0.107 

predicted_distance = function(x) {
  y = 21.033*x - 0.107
  return(y)
}

predicted_distance(median)
predicted_distance(x_95)

#we can't really convert back to meters because we log transformed + 1, but these predictions do look reasonable 
#The prediction intervals ?? I'm so confused. Fat bees fly longer according to these predictions. 






