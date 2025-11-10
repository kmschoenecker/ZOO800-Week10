##### Week 10 Homework ----
# Kristine Schoenecker, Nathan Lin, Elizabeth Braatz

#### Load Libraries ----
library(readxl)
library(janitor)
library(tidyverse)
library(ggfortify)

#### Objective 1 ----

#### Part A 
# Using either your own data or data that you find in an online database,
#find two continuous variables that might reasonably be hypothesized to have a 
#causal association (i.e., one variable is clearly the response, Y, and the 
#other the predictor, X) and have sufficient numbers of paired observations (> 30)

#For our homework, we found this paper from Kenna et al (2021) in Functional Ecology
#title: Thermal flight performance reveals impact of warming on bumblebee foraging potential
#relevant since Elizabeth and I both work on bumble bees

#pull the data in, this paper stored the information through Dryad
#not entirely sure if Dryad has a dynamic query option in R, so we downloaded the file to use

bee_data <- read_excel("Copy_of_bee_data.xlsx")
bee_data = clean_names(bee_data) #convert to snake case, Elizabeth showed me this function, it is so convenient!

#a lot of the values in this dataset were recorded as characters, so let's fix that for our work later
#since we might compare bees by their ID, save ID as a factor

bee_data$bee_id <- as.factor(bee_data$bee_id)
bee_data$colony <- as.factor(bee_data$colony) #in case we compare by colony
bee_data$weight_of_bee_after_feeding_g <- as.numeric(bee_data$weight_of_bee_after_feeding_g)
#it looks like the above was saved as a character initially because of NA values
bee_data$weight_out_of_nest_g <- as.numeric(bee_data$weight_out_of_nest_g)
bee_data$total_distance_m <- as.numeric(bee_data$total_distance_m)
bee_data$weight_of_bee_after_flight_g <- as.numeric(bee_data$weight_of_bee_after_flight_g)

#### Part B Using lm(), fit a linear regression to these data
#I would expect there to be a linear relationship between the difference in the weight of the bee
#before and after flight with the distance flown, bees that fly further should have a greater
#difference in weight

#bees that flew further, probably have a larger difference in weight
#add a column to our data for the difference in bee weight

bee_data$diff_weight <- bee_data$weight_out_of_nest_g - bee_data$weight_of_bee_after_flight_g
#bees that have a negative value in this new column gained weight after flight, right?

model_1 <- lm(diff_weight ~ total_distance_m, data = bee_data)

#scatterplot of this relationship, and we can add the model regression to this plot

scatter_1 <- ggplot(data = bee_data, mapping = aes(x = total_distance_m, y = diff_weight)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")

scatter_1

#### Part C

#use the plot() function to look at the various assumptions and see how well our model holds up

plot(model_1)
#first plot generated is residuals vs fitted
#we see a couple of outliers in this chart, not great.
#Also, most of our data is clustered near the left side of the graph

#the second plot generated is a QQ plot
#The middle of the plot seems to follow a straight line, which is great
#But neither the high or low end of the QQ plot follow that straight line, not great
#This means, I believe, that we do not have a normal distribution going on at the ends of our relationship

#the third plot is scale-location
#this one again shows some outliers, the red line is sort of horizontal, so I am guessing our
#assumption of heteroscedasicity is not too terribly violated?

#the fourth plot is the residuals vs leverage
#none of the points appear to be outside the dashed line that would indicate that
#they would highly influence the data? Is that the correct interpretation?

#### Part D Generate predictions and prediction intervals for the median and 95th percentile of X

#we can use the summary function to get the median

summary(bee_data$total_distance_m)
#median distance flown was 101.76 m
#to get the 95th quantile
quantile(bee_data$total_distance_m, probs = 0.95, na.rm = TRUE)
#the 95th quantile is 4080.576, whoa. That is really different from the median

#save these values in a new data frame

newdata <- data.frame(c(101.76, 4080.576))

#use predict()

predict(model_1, newdata)

#use the predict() function to generate predictions
#I am actually a little confused on how to do this
#in the examples I was looking at online, it seems like you end up putting the model into the predict
#function and then the data you have as new_data? 
#but if we enter the x, y values from the data, how do we get predictions?

# Generate prediction using our lm
predict.lm(model_1, interval="prediction")
#I am not sure why this prediction interval is not working for me
#I keep getting the error, 'total_distance_m' not found
#is this because I do not have it listed in the dataframe?
#if I just leave model_1, it does not seem to work, well it predicts for every single value

#### Objective 2 ----

#### A: making linear regression data without normally-distributed Y

# I'll use negative binomial, we make 100 xy pairs with error
# For x values I'll do a standard normal
sim_x_values <- rnorm(100)
# Set the slope and intercept
true_slope <- 4
true_intercept <- 8
# For error values I'll do a standard lognormal
sim_error_values_lnorm <- rlnorm(100)
# See what the y values look like, yeah it's very much not normal
hist(sim_error_values_lnorm)

# Calculate the y values from everything
sim_y_values <- true_intercept + (true_slope * sim_x_values) + sim_error_values_lnorm

# Just visualize, yeah it's linear
ggplot(mapping=aes(x=sim_x_values, y=sim_y_values)) + geom_point()

#### B: fit a linear model
sim_lm_object <- lm(sim_y_values ~ sim_x_values)
sim_lm_object

#### C: unsure what to be repeating but i'll pull out slope and intercept
lm_slope <- sim_lm_object$coefficients["sim_x_values"] 
lm_intercept <- sim_lm_object$coefficients["(Intercept)"]

# In case this is asking to repeat but with normally-distributed errors I'll do that
sim_error_values_norm <- rnorm(100)
sim_y_values_norm <- true_intercept + (true_slope * sim_x_values) + sim_error_values_norm
sim_lm_norm <- lm(sim_y_values_norm ~ sim_x_values)
sim_lm_norm

lm_slope_norm <- sim_lm_norm$coefficients["sim_x_values"]
lm_intercept_norm <- sim_lm_norm$coefficients["(Intercept)"]

#### D: how do the true and estimated slopes and intercepts compare?
# For the lnorm errors:
# The true slope is 4, while the calculated slope was 3.672. Not perfect but not bad!
# The true intercept is 8, while the calculated intercept was 9.512. It's close!

# For the norm errors:
# The calculated slope and intercept were 3.916 and 7.919. Much better here!

#### E: 95% prediction intervals for each x value
# For the lnorm errors simulation:
lnorm_sim_95_pred_intervals <- predict.lm(sim_lm_object, data.frame(x=sim_x_values), interval="prediction")

# For the norm errors simulation:
norm_sim_95_pred_intervals <- predict.lm(sim_lm_norm, data.frame(x=sim_x_values), interval="prediction")

#### F: what fraction falls in there?
# For lnorm errors simulation: 95 out of 100 y values fell in the prediction interval
sum((sim_y_values <= lnorm_sim_95_pred_intervals[, 3] & sim_y_values >= lnorm_sim_95_pred_intervals[, 2]))

# For norm errors simulation: 96 out of 100 y values fell in the prediction interval
sum((sim_y_values_norm <= norm_sim_95_pred_intervals[, 3] & sim_y_values_norm >= norm_sim_95_pred_intervals[, 2]))

#### G implications in estimated uncertainty vs true uncertainty
# Perhaps estimates of uncertainty are a little worse for log normal than normal,
# but I can't say by how much. It actually seemed like the parameter estimation was
# worse than the prediction interval performance here, but I'm not sure how much this
# depends on the given slope and intercept and error and x distributions.