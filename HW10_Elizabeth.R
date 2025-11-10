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
plot(d$log_distance~d$weight_out_of_nest_g)

#B fitting a linear regression 
#--------------------------------------------------------------
lm_object = lm(d$log_distance~d$weight_out_of_nest_g) #forcing it into linear, log midified 
#mod <-lm(d$total_distance_m ~ d$weight_out_of_nest_g + I(d$weight_out_of_nest_g^2), data = d) #if we assume quaadratic 
#borrowing terminology from Nathan's homework 

abline(reg, col = 'red', lwd = 3)
summary(lm_object)

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

hist(stdRes) #OK it's actually okay good as a log transform 

autoplot(lm_object) #Wow this doe sit all at once! Thanks Nathan!!! 



#D Making predictions 
#------------------------------------------------------------------------------

#how far does our model think bees of a median weight, and super fat bees at the 95th percentile, will fly? 

median = median(d$weight_out_of_nest_g)
x_95 = quantile(d$weight_out_of_nest_g, 0.95)

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
# OK trying to actually use it based off Nathan's stuff 
#############################################################

#-----------------Nathan's stuff 

# For the median value of X
# Pull out median value
start_median <- median(cleaned_bees$weight_start)
# Generate prediction using our lm
predict.lm(lm_object, data.frame(weight_start=start_median), interval="prediction")
# Our fitted value is 0.221, with prediction interval [0.1664, 0.2747]

# For the 95th percentile of X
# Pull out 95th percentile
start_95th <- quantile(x=cleaned_bees$weight_start, probs=(0.95))
# Generate prediction using our lm
predict.lm(lm_object, data.frame(weight_start=start_95th), interval="prediction")
# Our fitted value is 0.282, with prediction interval [0.2275, 0.3362]

# ------------ Kristine's stuff 

summary(bee_data$total_distance_m)
#median distance flown was 101.76 m
#to get the 95th quantile
quantile(bee_data$total_distance_m, probs = 0.95, na.rm = TRUE)
#the 95th quantile is 4080.576, whoa. That is really different from the median

predict.lm(model_1, interval="prediction")
#I am not sure why this prediction interval is not working for me
#I keep getting the error, 'total_distance_m' not found
#is this because I do not have it listed in the dataframe?
#if I just leave model_1, it does not seem to work, well it predicts for every single value

#Trying to help Kristine 


###########################################################################
#I couldn't figure out the predict function, so I very hackily made my own 
############################################################################



######################################
#    Objective 2
######################################

#ok, we need a bunch of x y pairs 

#from ai, thits generates completely random pairs 
# Set a seed for reproducibility
set.seed(456)

# Number of pairs to generate
n_pairs <- 10

# Generate random x-coordinates from a normal distribution (mean=5, sd=2)
x_coords_norm <- rnorm(n_pairs, mean = 5, sd = 2)

# Generate random y-coordinates from a normal distribution (mean=5, sd=2)
y_coords_norm <- rnorm(x_coords_norm*2, mean = 5, sd = 2)

# Combine into a data frame
random_pairs_normal <- data.frame(x = x_coords_norm, y = y_coords_norm)

# Print the resulting pairs
print(random_pairs_normal)
