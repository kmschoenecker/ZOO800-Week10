##### Week 10 Homework ----
# Kristine Schoenecker, Nathan Lin, Elizabeth Braatz

#### Load Libraries ----
library(readxl)
library(janitor)
library(tidyverse)

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

#use the predict() function to generate predictions
#I am actually a little confused on how to do this
#in the examples I was looking at online, it seems like you end up putting the model into the predict
#function and then the data you have as new_data? 
#but if we enter the x, y values from the data, how do we get predictions?

#here is how Nathan did the predictions

start_median <- median(bee_data$total_distance_m, na.rm = TRUE)
start_median
# Generate prediction using our lm
predict.lm(model_1, interval="prediction")
#I am not sure why this prediction interval is not working for me
#I keep getting the error, 'total_distance_m' not found
#is this because I do not have it listed in the dataframe?
#if I just leave model_1, it does not seem to work, well it predicts for every single value

#### Objective 2 ----

#### Part A Generate linear regression data where the error in the response variable Y is not quite normally
#distributed (but still unimodal). A lognormal or negative binomial distribution should work. No
#error in X this time. 100 X, Y pairs should be good.

#by no error in X, does that mean generate from a normal distribution?

X <- rnorm(100)
Y <- rlnorm(100) #to generate log normal values

#### Part B Fit a linear regression to this data

model_2 <- lm(Y ~ X)
summary(model_2)

plot(model_2)

#### Part C

intercept <- coef(model_2)[1]
slope <- coef(model_2)[2] #pulls out and saves the slope from our model

#what do you mean, true value for the slope and intercept?
#like, just if we had plotted them as a scatterplot the slope and intercept?

#### Part D
#### Part E
#### Part F
#### Part G