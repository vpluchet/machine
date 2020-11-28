library(tidyverse)
library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)
avg

mean((avg - test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef

# predicting sons heights in test set
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

# predicting the same, using the predict function
test <- predict(fit, newdata = test_set, se.fit = FALSE)
mean((test - test_set$son)^2)

# read help files
?predict.lm
?predict.glm

## example from Venables and Ripley (2002, pp. 190-2.)
ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive = 20-numdead)
budworm.lg <- glm(SF ~ sex*ldose, family = binomial)
summary(budworm.lg)

plot(c(1,32), c(0,1), type = "n", xlab = "dose",
     ylab = "prob", log = "x")
text(2^ldose, numdead/20, as.character(sex))
ld <- seq(0, 5, 0.1)
lines(2^ld, predict(budworm.lg, data.frame(ldose = ld,
                                           sex = factor(rep("M", length(ld)), levels = levels(sex))),
                    type = "response"))
lines(2^ld, predict(budworm.lg, data.frame(ldose = ld,
                                           sex = factor(rep("F", length(ld)), levels = levels(sex))),
                    type = "response"))

# Regression test
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# We will build 100 linear models using the data above and calculate the mean
# and standard deviation of the combined models. First, set the seed to 1 again
# (make sure to use sample.kind="Rounding" if your R is version 3.6 or later). 
# Then, within a replicate() loop, (1) partition the dataset into test and training 
# sets with p = 0.5 and using dat$y to generate your indices, (2) train a linear model 
# predicting y from x, (3) generate predictions on the test set, and (4) calculate the RMSE 
# of that model. Then, report the mean and standard deviation (SD) of the RMSEs from all 100 models.

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

RMSE <- replicate(n = n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

c(mean = mean(RMSE), sd = sd(RMSE))

# Now we will repeat the exercise above but using larger datasets. 
# Write a function that takes a size n, then (1) builds a dataset
# using the code provided at the top of Q1 but with n observations instead of 100
# and without the set.seed(1), (2) runs the replicate() loop that you wrote to
# answer Q1, which builds 100 linear models and returns a vector of RMSEs,
# and (3) calculates the mean and standard deviation of the 100 RMSEs.
# Set the seed to 1 (if using R 3.6 or later, use the argument sample.kind="Rounding") 
# and then use sapply() or map() to apply your new function to n <- c(100, 500, 1000, 5000, 10000).

Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)

simul <- function(N){
  dat <- MASS::mvrnorm(n = N, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  RMSE <- replicate(n = 100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
    c(mean = mean(RMSE), sd = sd(RMSE))
}

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

out <- simul(100)
out

n <- c(100, 500, 1000, 5000, 10000)
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
sapply(n, simul)

# Now with higher correlation
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

RMSE <- replicate(n = n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

c(mean = mean(RMSE), sd = sd(RMSE))

# Correlation matrix with two predictors
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit1 <- lm(y ~ x_1, data = train_set)
fit2 <- lm(y ~ x_2, data = train_set)
fit12 <- lm(y ~ x_1 + x_2, data = train_set)

y_1 <- predict(fit1, newdata = test_set)
y_2 <- predict(fit2, newdata = test_set)
y_12 <- predict(fit12, newdata = test_set)

res1 <- sqrt(mean((y_1 - test_set$y)^2))
res2 <- sqrt(mean((y_2 - test_set$y)^2))
res12 <- sqrt(mean((y_12 - test_set$y)^2))

c(res1 = res1, res2 = res2, res12 = res12)


# Repeat the exercise from Q6 but now create an example in which x_1 and x_2 are highly correlated.
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit1 <- lm(y ~ x_1, data = train_set)
fit2 <- lm(y ~ x_2, data = train_set)
fit12 <- lm(y ~ x_1 + x_2, data = train_set)

y_1 <- predict(fit1, newdata = test_set)
y_2 <- predict(fit2, newdata = test_set)
y_12 <- predict(fit12, newdata = test_set)

res1 <- sqrt(mean((y_1 - test_set$y)^2))
res2 <- sqrt(mean((y_2 - test_set$y)^2))
res12 <- sqrt(mean((y_12 - test_set$y)^2))

c(res1 = res1, res2 = res2, res12 = res12)







