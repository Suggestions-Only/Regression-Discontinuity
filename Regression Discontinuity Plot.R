#Regression Discontinuity Plot
require(readr)
require(ggplot2)
require(tidyverse)
ammus <- read_csv("~/School Work/Undergraduate Repository/Theories of Personality/Test 4/Test 4 Data File.csv")

##Dummy Coding
ammus$over_avg_age <- ammus$AGE >= 25.34

##Creating Model Prediction Values
model1 <- lm(Avoidant_Movie_Behavior ~ AGE + over_avg_age,
             data = ammus)
model_val <- predict(model1, data = ammus)

##Initial Graph
my_graph1 <- ggplot(data = ammus, aes(x = AGE, y = Avoidant_Movie_Behavior,
                                      color = over_avg_age)) +
  geom_point() +
  stat_smooth(method = lm)
my_graph1

##LOESS ggplot2 Graph
my_graph2 <- ggplot(data = ammus, aes(x = AGE, y = Avoidant_Movie_Behavior,
                                  color = over_avg_age)) +
  geom_point() +
  stat_smooth(method = loess)
my_graph2

#numerical analysis
below <- filter(ammus, AGE, Avoidant_Movie_Behavior, over_avg_age, over_avg_age == FALSE)

model.below <- lm(Avoidant_Movie_Behavior ~ AGE, data = below)
summary(model.below)

above <- filter(ammus, AGE, Avoidant_Movie_Behavior, over_avg_age, over_avg_age == TRUE)

model.above <- lm(Avoidant_Movie_Behavior ~ AGE, data = above)
summary(model.above)
