library(tidyverse)
library(dslabs)
library(gtools)

data(heights)
heights

class(heights)
class(heights$sex)
class(heights$height)
class("Male")
class(75.00000)

nrow(heights)
heights$height[777]
heights$sex[777]
heights[1,777]
heights[777,1]

max(heights$height)
which.min(heights$height)

mean(heights$height)

median(heights$height)

mean(heights$sex == "Male")

sum(heights$sex == "Male")
sum(heights$sex == "Female")

sum(heights$height > 78)

heights %>% filter(sex == "Female" & height>78) %>% nrow(.)

mnist <- read_mnist()
head(mnist)
ncol(mnist$train$images)

