library(caret)
library(party)
library(tidyverse)
library(readr)

FullSurvey <- read.csv(file = "C:/Users/ASUS/Documents/Ubiqum/2.2/Survey_Key_and_Complete_Responses_excel.csv", header = TRUE, sep = ",")
summary(FullSurvey)
str(FullSurvey)
FullSurvey$brand <- as.factor(FullSurvey$brand)
FullSurvey$elevel <- as.factor(FullSurvey$elevel)
FullSurvey$zipcode <- as.factor(FullSurvey$zipcode)
FullSurvey$car <- as.factor(FullSurvey$car)
levels(FullSurvey$brand) <- c("Acer", "Sony")
str(FullSurvey)
tree <- ctree(brand ~ ., FullSurvey, controls = ctree_control(maxdepth = 3))
plot(tree)

plot(salary ~ brand, data = FullSurvey)
Acer <- subset(FullSurvey, brand == "Acer")
Sony <- subset(FullSurvey, brand == "Sony")
plot(salary ~ age, data = Acer, col = "red")
points(salary ~ age, data = Sony, col = "blue")

set.seed(123)
inTrain <- createDataPartition(
  y = FullSurvey$brand,
  p = .75,
  list = FALSE
)
training <- FullSurvey[inTrain,]
testing <- FullSurvey[-inTrain,]

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
knn.model4 <- train(brand ~ ., data = training, method = "knn", trControl = fitControl, tuneLength = 30)
knn.model4
predictors(knn.model4)
knn4.pred <- predict(knn.model4, newdata = testing)
postResample(knn4.pred, testing$brand)

fitControl2 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
rf.model5 <- train (brand ~ ., data = training, method = "ranger", trControl = fitControl2, tuneLength = 10)
rf.model5
rf.pred5 <- predict(rf.model5, newdata = testing)
confusionMatrix(data = rf.pred5, testing$brand)
plot(brand ~ salary, data = testing)
plot(rf.pred5 ~ salary, data = testing)
postResample(rf.pred5, testing$brand)
plot(rf.pred5,testing$brand, ylab = "real brand", xlab = "predicted brand")

Incomplete <- read.csv(file = "C:/Users/ASUS/Documents/Ubiqum/2.2/SurveyIncomplete.csv", header = TRUE, sep = ",")
Incomplete$brand <- as.factor(Incomplete$brand)
Incomplete$elevel <- as.factor(Incomplete$elevel)
Incomplete$zipcode <- as.factor(Incomplete$zipcode)
Incomplete$car <- as.factor(Incomplete$car)

Completed <- predict(rf.model5, newdata = Incomplete)
Incomplete$brand <- Completed

setwd("C:/Users/ASUS/Documents/Ubiqum/2.2")
write.csv(Incomplete, file = "Completed2.csv")
