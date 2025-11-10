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

#scatterplot of this relationship

scatter_1 <- ggplot(data = bee_data, mapping = aes(x = total_distance_m, y = diff_weight)) +
  geom_point()
scatter_1

#### Part C
#### Part D

#### Objective 2 ----

#### Part A
#### Part B
#### Part C