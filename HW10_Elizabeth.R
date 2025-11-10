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

y = log(d$total_distance_m + 1) #changing distance to log + 1, not great but not as bad 
hist(y)

d$log_distance = log(d$total_distance_m + 1)

#i think it's actually quadratic 

#here's a model for a quadratic relatoinship
mod <-lm(d$total_distance_m ~ d$weight_out_of_nest_g + I(d$weight_out_of_nest_g^2), data = d)


#Assumption 1 Linearity

#Scatterplot of bee weight vs. distance 
plot(d$weight_out_of_nest_g ~ d$log_distance)

#B fitting a linear regression 
#--------------------------------------------------------------
lm_object = lm(weight_out_of_nest_g ~ log_distance, data = d) # my model 
#mod <-lm(d$total_distance_m ~ d$weight_out_of_nest_g + I(d$weight_out_of_nest_g^2), data = d) #if we assume quaadratic 
#borrowing terminology from Nathan's homework 

abline(lm_object, col = 'red', lwd = 3)
summary(lm_object)

#C still checking if it's linear  
#------------------------------------------------------------
predicted = predict.lm(lm_object) 
residuals = d$weight_out_of_nest_g[1:331] - predicted

##  plot residuals vs. the predicted values:
plot(predicted,residuals,cex=2,cex.lab=1.5,cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")
#not great 

###Assumption 2 Homogeneity of Variances
#We look at the same plot of residuals vs. the predicted values

#Assumption 3 Normality
#calculate standardized residuals
stdRes = rstandard(lm_object)

#QQPLOT
qqnorm(stdRes,ylab="Standardized Residuals", xlab="Theoretical Quantiles")
qqline(stdRes, col=2,lwd=2) #not amazing but not terrible 

#historgram of std residuals

hist(stdRes) #OK it's actually okay good as a log transform 

autoplot(lm_object) #Wow this doe sit all at once! Thanks Nathan!!! 

#D Making predictions 
#------------------------------------------------------------------------------

#how far does our model think bees of a median weight, and super fat bees at the 95th percentile, will fly? 

median = median(d$weight_out_of_nest_g); median
x_95 = quantile(d$weight_out_of_nest_g, 0.95); x_95

###########################################################################
#I couldn't figure out the predict function, so I very hackily made my own 
############################################################################

reg$coefficients #extract our coefficients from our linear model 
 # y = mx + b 
 # distance = 21.033(weight out of nest) + -0.107 

predicted_distance = function(x) {          #creating a function to see predicted distance given weight
  y = 21.033*x - 0.107                      #log (distance + 1)
  y = exp(y)                                #distance in m 
  return(y)                                 #what to return 
}

#predicted distance in meters 
predicted_distance(median)
predicted_distance(x_95)

# these predictions do look reasonable 
#The prediction intervals ?? I'm so confused. Fat bees fly longer according to these predictions. 

#############################################################
# OK trying to actually use it based off Nathan and Kristine's stuff 
#############################################################

library(tidyverse)
library(ggfortify)

#my stuff (yay works now!) 
lm_object = lm(weight_out_of_nest_g ~ log_distance, data = d) # my model 
summary(d$log_distance) #median was 4.632 
quantile(d$log_distance, probs = 0.95, na.rm = TRUE) #95th percentile was 8.314166 
new_df = data.frame(log_distance = c(4.632, 8.314166))
predict(lm_object, new_df, interval = "prediction") # yay now it works 

######################################
#    Objective 2
######################################

#A: Generate linear regression data where the error in the response variable Y is not quite normally distributed (but still unimodal).  A lognormal or negative binomial distribution should work.  No error in X this time. 100 X, Y pairs should be good.
#ok, we need a bunch of x y pairs 


#from ai, thits generates completely random pairs 
# Set a seed for reproducibility
set.seed(456)

# Number of pairs to generate
n_pairs <- 100

# Generate random x-coordinates from a normal distribution (mean=5, sd=2)
sim_x_values <- rnorm(n_pairs, mean = 5, sd = 2)

#what do we want our line to be? (pulling from Nathan's code) 
#y = mx + b 
#y = 4x + 8  

# Set the slope and intercept
true_slope <- 4
true_intercept <- 8
sim_errors = rlnorm(100, meanlog = 0, sdlog = 1) #generate jitter (random variation to add to the y variable. Oh, these are all positive, we are going to want both positive and negative jitters) 

#Generate y values  
sim_y_values <- true_intercept + (true_slope * sim_x_values) + sim_errors

# Just visualize, yeah it's linear (stolen from Nathan) 
ggplot(mapping=aes(x=sim_x_values, y=sim_y_values)) + geom_point()

# Combine into a data frame
df <- data.frame(x = sim_x_values, y = sim_y_values)

#B: Fit a linear regression to the data


lm_rlnorm = lm( y ~ x, data = df)
plot( y ~ x, data = df)
abline(lm_rlnorm, col = 'red', lwd = 3)
summary(lm_rlnorm)

#C: Repeat this process and keep track of the true and estimated slope and intercept. 
# I'm confused, are we comparing to the true slope, the rlnorm slope, or like Nathan said a new rnorm dataset? 



#Generate perfect y values  
perfect_y_values <- true_intercept + (true_slope * sim_x_values) 
names(df)[2] = "y_sim_rlnorm" #rename our simulated column 
df$y_perfect = perfect_y_values

#fit linear regression to the perfect data 
lm_perfect = lm(y_perfect ~ x, data = df)
plot(y_perfect~x, data = df)
abline(lm_perfect, col = "red", lwd = 2)
summary(lm_perfect)

#rnorm plot 

sim_errors_norm = rnorm(100) #generate jitter (random variation to add to the y variable, normally distributed) 

#Generate y values  
sim_y_values_norm <- true_intercept + (true_slope * sim_x_values) + sim_errors_norm

# Just visualize, yeah it's linear (stolen from Nathan) 
ggplot(mapping=aes(x=sim_x_values, y=sim_y_values_norm)) + geom_point()

#fit linear regression to the perfect data 
lm_rnorm = lm(sim_y_values_norm ~ x, data = df)
plot(sim_y_values_norm~x, data = df)
abline(lm_rnorm, col = "red", lwd = 2)
summary(lm_rnorm)

# Combine into a data frame
df$y_sim_rnorm = sim_y_values_norm


# D.	How well do the estimated slope and intercept match the true values?


#extract coefficients
lm_rlnorm$coefficients
lm_perfect$coefficients
lm_rnorm$coefficients

#The rnorm was closer to the true values than the rlnorm, though both were pretty close 
#I dunno, Nathan did them, I think we need to do the same thing we did in part 1 but for all the data 

#E. 95th quantiles 

lnorm_sim_pred_intervals = predict.lm()
#my stuff (yay works now!) 
lm_object = lm(weight_out_of_nest_g ~ log_distance, data = d) # my model 
summary(d$log_distance) #median was 4.632 
quantile(d$log_distance, probs = 0.95, na.rm = TRUE) #95th percentile was 8.314166 
new_df = data.frame(log_distance = c(4.632, 8.314166))
predict(lm_object, new_df, interval = "prediction") # yay now it works 


# For the lnorm errors simulation:
lnorm_sim_95_pred_intervals <- predict.lm(sim_lm_object, data.frame(x=sim_x_values), interval="prediction")

# For the norm errors simulation:
norm_sim_95_pred_intervals <- predict.lm(sim_lm_norm, data.frame(x=sim_x_values), interval="prediction")
