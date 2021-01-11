library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

class(titanic_clean)
?titanic_train
titanic_clean %>% group_by(Sex) %>% summarise (n = n(), survp = mean(Survived == 1))
titanic_clean %>% group_by(Pclass, Sex) %>% summarise (n = n(), survp = mean(Survived == 1))
titanic_clean %>% filter(Pclass == 3) %>% ggplot(aes(Fare)) + geom_histogram(binwidth = 1)

set.seed(42, sample.kind = "Rounding")
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
train_set <- titanic_clean %>% slice(-test_index)
test_set <- titanic_clean %>% slice(test_index)

mean(train_set$Survived == 1)

# Guessing
set.seed(3, sample.kind = "Rounding")
guess <- sample(x = c(0,1), size = nrow(test_set), replace = TRUE)
mean(guess == test_set$Survived)

# Survival proportions in training set
train_set %>% group_by(Sex) %>% summarise(survival_rate = mean(Survived == 1))
# Using proportions to predict
sex_model <- if_else(test_set$Sex == "female", 1, 0) # predict Survived=1 if female, 0 if male
mean(sex_model == test_set$Survived) # calculate accuracy

train_set %>% group_by(Pclass) %>% summarise(survival_rate = mean(Survived == 1))
class_model <-  if_else(test_set$Pclass == 1, 1, 0) # predict Survived=1 if first class
mean(class_model == test_set$Survived) # calculate accuracy

t <- train_set %>% group_by(Sex, Pclass) %>% summarise(survival_rate = mean(Survived == 1))
t
t %>% filter(survival_rate > 0.5)

sex_class_model <- if_else(test_set$Sex == "female" & test_set$Pclass %in% c(1,2), 1, 0)
mean(sex_class_model == test_set$Survived)


confusionMatrix(factor(guess), reference = factor(test_set$Survived))
confusionMatrix(factor(sex_model), reference = factor(test_set$Survived))
confusionMatrix(factor(sex_class_model), reference = factor(test_set$Survived))

F_meas(factor(guess), reference = factor(test_set$Survived))
F_meas(factor(sex_model), reference = factor(test_set$Survived))
F_meas(factor(sex_class_model), reference = factor(test_set$Survived))


# LDA Model
set.seed(1, sample.kind = "Rounding")
train_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
lda_preds <- predict(train_lda, test_set)
mean(lda_preds == test_set$Survived)

set.seed(1, sample.kind = "Rounding")
train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
qda_preds <- predict(train_qda, test_set)
mean(qda_preds == test_set$Survived)

# GLM model
set.seed(1, sample.kind = "Rounding")
train_glm <- train(Survived ~ Age, method = "glm", data = train_set)
glm_preds <- predict(train_glm, test_set)
mean(glm_preds == test_set$Survived)

set.seed(1, sample.kind = "Rounding")
train_glm4 <- train(Survived ~ Sex+Pclass+Fare+Age, method = "glm", data = train_set)
glm4_preds <- predict(train_glm4, test_set)
mean(glm4_preds == test_set$Survived)

set.seed(1, sample.kind = "Rounding")
train_glma <- train(Survived ~ ., method = "glm", data = train_set)
glma_preds <- predict(train_glma, test_set)
mean(glma_preds == test_set$Survived)

# KNN model
set.seed(6, sample.kind = "Rounding")
grid <- data.frame(k = seq(3, 51, 2))
train_knn <- train(Survived ~ ., 
                   method = "knn",
                   tuneGrid = grid,
                   data = train_set)
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune

knn_preds <- predict(train_knn, test_set)
mean(knn_preds == test_set$Survived)

# Cross-validation
set.seed(8, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = .9)
train_knncv <- train(Survived ~ ., 
                   method = "knn",
                   tuneGrid = grid,
                   trControl = control,
                   data = train_set)
ggplot(train_knncv, highlight = TRUE)
train_knncv$bestTune

knncv_preds <- predict(train_knncv, test_set)
mean(knncv_preds == test_set$Survived)

# Rpart method
library(rpart)
modelLookup("rpart")
cpg <- data.frame(cp = seq(0, 0.05, 0.002))
set.seed(10, sample.kind = "Rounding")
train_rpart <- train(Survived ~ .,
                     method = "rpart",
                     tuneGrid = cpg,
                     data = train_set)
train_rpart$bestTune
rpart_preds <- predict(train_rpart, test_set)
mean(rpart_preds == test_set$Survived)

plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.7)
library(rattle)
fancyRpartPlot(train_rpart$finalModel, yesno = 2)

train_rpart$finalModel

mean(train_set$Survived == 1) # Overall survival rate is 38%

# Left part of the tree
train_set %>% summarise(n = sum(Sex == "male"), prop = mean(Sex == "male")) #463 males, 65% of the total
train_set %>% filter(Sex == "male") %>% summarise(prop = mean(Survived == 1)) #20% of males survived
train_set %>% summarise(prop = mean(Age >= 3.5 & Sex == "male")) #63% of passengers were aged >3.5
train_set %>% filter(Sex == "male" & Age >= 3.5) %>% summarise(prop = mean(Survived == 1)) #18% survival
# for males >= 3.5 years
train_set %>% filter(Sex == "male" & Age < 3.5) %>% summarise(prop = mean(Survived == 1)) #79% survival
# for males < 3.5 years

#Right part of the tree
train_set %>% summarise(n = sum(Sex == "female"), prop = mean(Sex == "female")) #249 females, 35% of the total
train_set %>% filter(Sex == "female") %>% summarise(prop = mean(Survived == 1)) #73% of females survived
train_set %>% summarise(prop = mean(Pclass %in% c(1,2) & Sex == "female")) #18% of passengers were Females in classes 1-2
train_set %>% filter(Sex == "female" & Pclass != 3) %>% summarise(prop = mean(Survived == 1)) #94% survived in Females Classes 1-2
train_set %>% summarise(prop = mean(Pclass == 3 & Sex == "female")) #17% of passengers are Females in class 3
train_set %>% filter(Sex == "female" & Pclass == 3) %>% summarise(prop = mean(Survived == 1)) #50% survived in Females in class 3
train_set %>% summarise(prop = mean(Sex == "female" & Pclass == 3 & Fare >= 23.35)) #3% of passengers are Females in class 3 with Fare >23.35
train_set %>% filter(Sex == "female" & Pclass == 3 & Fare >= 23.35) %>% summarise(prop = mean(Survived == 1)) #12% of Females in class 3 with Fare >23.35 survived
train_set %>% summarise(prop = mean(Sex == "female" & Pclass == 3 & Fare < 23.35)) #13% of passengers are Females in class 3 with Fare <23.35
train_set %>% filter(Sex == "female" & Pclass == 3 & Fare < 23.35) %>% summarise(prop = mean(Survived == 1)) #60% of Females in class 3 with Fare <23.35 survived

train_set %>% filter(Sex == "male" & SibSp == 4)
train_set %>% filter(Sex == "male" & SibSp == 2 & Pclass == 1)

library(rpart.plot)
rpart.plot(x = train_rpart$finalModel, 
           type = 2, 
           extra = 0)

our_test_set <- data.frame(
  Sex = c("male", "female", "female", "male", "female", "female", "male"),
  Age = c(28,20,20,5,20,20,17),
  Pclass = as.integer(c(1,2,3,1,3,1,1)),
  Fare = c(1.00,1.00,8.00,1.00,25.00,1.00,1.00),
  SibSp = rep(1,7),
  Parch = as.integer(rep(1,7)),
  FamilySize = rep(1,7),
  Embarked = factor( sample(levels(test_set$Embarked),size = 7, replace = T) ) )

predict(train_rpart, our_test_set)


# Random forest model
library(randomForest)
modelLookup("rf")
fgrid <- data.frame(mtry = seq(1:7))
set.seed(14, sample.kind = "Rounding")
train_rf <- train(Survived ~ .,
                    method = "rf",
                    tuneGrid = fgrid,
                    ntree = 100,
                    data = train_set)
train_rf$bestTune
rf_preds <- predict(train_rf, test_set)
mean(rf_preds == test_set$Survived)

varImp(train_rf)
