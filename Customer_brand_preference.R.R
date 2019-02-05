# ------ Customer brand preference ----- #
# developer: Afroditi Doriti
# version: 2.0
# changes: used doParallel
# description: customer brand preferences, classification models
# input: "Survey_Key_and_Complete_Responses_excel.csv", "SurveyIncomplete.csv"
# data ownership: Ubiqum code academy
# ------------------------------- #
# outputs: "SurveyCompleted.csv"
# ------------------------------- #

# load packages ----
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load("tidyverse",
               "caret",
               "party",
               "doParallel")

# doParallel ----
# Choose a number of cores to use (1 less than detected)
no_cores <- detectCores() - 1

# Create Cluster with desired number of cores. Don't use them all! Your computer is running other processes.
cl <- makeCluster(no_cores)

# Register Cluster
registerDoParallel(cl)

# to onfirm how many cores are now "assigned" to R and RStudio
# getDoParWorkers()

# set wd and read file ----
# set wd
setwd("/home/adoriti/Documents/Ubiqum")

# choose input file
input_file <- "Survey_Key_and_Complete_Responses_excel.csv"
input_file2 <- "SurveyIncomplete.csv"

# read file
FullSurvey <- read.csv(input_file, header = TRUE, sep = ",")

# Preprocessing ----
# make into factors
FullSurvey$brand <- as.factor(FullSurvey$brand)
FullSurvey$elevel <- as.factor(FullSurvey$elevel)
FullSurvey$zipcode <- as.factor(FullSurvey$zipcode)
FullSurvey$car <- as.factor(FullSurvey$car)

# set the two brands as the levels of brand
levels(FullSurvey$brand) <- c("Acer", "Sony")

# (optional) check tree
# tree <- ctree(brand ~ ., FullSurvey, controls = ctree_control(maxdepth = 3))
# plot(tree)

# Models ----
# set seed
set.seed(123)

# create data partition
inTrain <- createDataPartition(y = FullSurvey$brand,
                               p = .75,
                               list = FALSE)

# create training and testing sets
training <- FullSurvey[inTrain, ]
testing <- FullSurvey[-inTrain, ]

# knn
# create train control for knn
fitControl <-
  trainControl(method = "repeatedcv",
               number = 10,
               repeats = 10)

# train knn model
knn_model <-
  train(
    brand ~ .,
    data = training,
    method = "knn",
    trControl = fitControl,
    tuneLength = 30
  )

# check knn_model
knn_model

# check predictors for knn_model
predictors(knn_model)

# prediction with knn_model for testing data
knn_pred <- predict(knn_model, newdata = testing)

# check accuracy
postResample(knn_pred, testing$brand)

# random forest
# Create train control for random forest
fitControl2 <-
  trainControl(method = "repeatedcv",
               number = 10,
               repeats = 1)

# train random forest model
rf_model <-
  train (
    brand ~ .,
    data = training,
    method = "ranger",
    trControl = fitControl2,
    tuneLength = 10
  )

# check random forest model
rf_model

# predict results using the random forest model for the testing set
rf_pred <- predict(rf_model, newdata = testing)

# check confustion matrix
confusionMatrix(data = rf_pred, testing$brand)

# check accuracy
postResample(rf_pred, testing$brand)

# plot results
plot(brand ~ salary, data = testing)
plot(rf.pred5 ~ salary, data = testing)
plot(rf_pred, testing$brand, ylab = "real brand", xlab = "predicted brand")

# Incomplete survey ----
# read incomplete survey
Incomplete <-
  read.csv(input_file2, header = TRUE, sep = ",")

# same preprocessing as for the full survey (make into factors)
Incomplete$brand <- as.factor(Incomplete$brand)
Incomplete$elevel <- as.factor(Incomplete$elevel)
Incomplete$zipcode <- as.factor(Incomplete$zipcode)
Incomplete$car <- as.factor(Incomplete$car)

# Prediction ----
# prediction for the incomplete survey
Completed <- predict(rf_model, newdata = Incomplete)
Incomplete$brand <- Completed

# save the resulting survey as csv
write.csv(Incomplete, file = "SurveyCompleted.csv")

# Stop Cluster ----
stopCluster(cl)
