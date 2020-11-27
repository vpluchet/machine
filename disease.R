library(tidyverse)
library(dslabs)
library(dplyr)

p_disease <- 0.02 # disease prevalence in population
q_disease <- 1- p_disease
t_sensitivity <- 0.85 # test sensitivity ie prob of positive test if disease
t_specificity <- 0.90 # test specificity ie prob of negative test if no disease
N <- 1e6 # population size

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
# Creating a 1m population with a 2% probability of having the disease (1 means disease)
disease <- sample(c(0,1), size=N, replace=TRUE, prob=c(q_disease, p_disease))
test <- rep(NA, N)
# Testing the population without the disease, test has 90% specificity; 1 means positive test
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, 
                           prob=c(t_specificity,1-t_specificity))
# Testing the population with the disease, test has 85% sensitivity; 1 means positive test
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, 
                           prob=c(1-t_sensitivity , t_sensitivity))

# Theoretical probability of positive test
t_positive <- t_sensitivity * p_disease + (1-t_specificity) * q_disease
t_positive
t_negative <- 1 - t_positive
# Proportion of positive tests
mean(test)

# Theoretical probability of having the disease if test is negative
(1 - t_sensitivity) * p_disease / t_negative
# Proportion of disease in negative test population
mean(disease[test == 0])

# Theoretical probability of having the disease if test is positive
t_sensitivity * p_disease / t_positive
# Proportion of disease in positive test population
mean(disease[test == 1])

# Comparing prevalence in population with positive test versus general population
# First calculate the probability of having the disease given a positive test, then divide
# by the probability of having the disease.
t_sensitivity / t_positive
mean(disease[test==1]==1)/mean(disease==1)



 