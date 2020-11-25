library(tidyverse)
library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)
library(e1071)
library(caret)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type
# note that there are two types: inclass and online

# Compute percentage of Females in each type 
dat %>% filter(type == "inclass") %>% summarise(prop = mean(sex == "Female"))
dat %>% filter(type == "online") %>% summarise(prop = mean(sex == "Female"))
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

# Predict Female if inclass as Females are more prevalent in inclass
y_hat <- ifelse(x == "inclass", "Female", "Male") %>% factor(levels = levels(y))
# compute overall accuracy
mean(y_hat == y)
table(predicted=y_hat, actual=y)
sensitivity(data = y_hat, reference = y)
specificity(data = y_hat, reference = y)
mean(dat$sex == "Female")

# Compute confusion matrix
confusionMatrix(data = y_hat, reference = y)

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

summary(iris)

# Creating test and train sets
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

cutoff <- seq(min(iris$Sepal.Length), max(iris$Sepal.Length), 0.1)
accuracy <- map_dbl(cutoff, function(z){
  y_hat <- ifelse(train$Sepal.Length > z, "virginica", "versicolor") %>%
  factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) +
  geom_point() + 
  geom_line() +
  ggtitle("Sepal Length")

cutoff <- seq(min(iris$Sepal.Width), max(iris$Sepal.Width), 0.1)
accuracy <- map_dbl(cutoff, function(z){
  y_hat <- ifelse(train$Sepal.Width > z, "virginica", "versicolor") %>%
  factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) +
  geom_point() + 
  geom_line() +
  ggtitle("Sepal Width")

cutoff <- seq(min(iris$Petal.Length), max(iris$Petal.Length), 0.1)
accuracy <- map_dbl(cutoff, function(z){
  y_hat <- ifelse(train$Petal.Length > z, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
cutoff[which.max(accuracy)]

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) +
  geom_point() + 
  geom_line() +
  ggtitle("Petal Length")

cutoff <- seq(min(iris$Petal.Width), max(iris$Petal.Width), 0.1)
accuracy <- map_dbl(cutoff, function(z){
  y_hat <- ifelse(train$Petal.Width > z, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
cutoff[which.max(accuracy)]

# Finding the predictor with highest accuracy with one piece of code
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)

# Applying the best cutoff to the train data set
z <- 4.7
y_hat <- ifelse(train$Petal.Length > z, "virginica", "versicolor") %>%
  factor(levels = levels(train$Species))
mean(y_hat == train$Species)

# Applying the best cutoff to test
y_hat <- ifelse(test$Petal.Length > z, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)

# Alternative code
predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]
y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)

# Finding the predictor with highest accuracy with the test data
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)

plot(iris,pch=21,bg=iris$Species)

# Applying the best cutoffs to the train data set
cutoff_PL <- 4.7
cutoff_PW <- 1.5

y_hat <- ifelse(test$Petal.Length > cutoff_PL | test$Petal.Width > cutoff_PW, 
                "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)










