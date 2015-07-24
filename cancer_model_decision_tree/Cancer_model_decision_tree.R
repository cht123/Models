source("~/Classes/DSS/Functions.R")

# check for required packages
pkg_ck(c('dplyr', 'class', 'ggvis', 'corrplot', 'gmodels', 'C50'))

setwd("~/Classes/Model_Building")


if (!file.exists("cancer_model_decision_tree")) {
  dir.create("cancer_model_decision_tree")
}
fileURL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile = "cancer_model_decision_tree/candata.csv", method = "curl")
list.files("./cancer_model_decision_tree")
dataDownloaded <- date()

#read table
cancerData <- read.csv("cancer_model_decision_tree/candata.csv", header = TRUE)

# add field names
names(cancerData) <- c("id", "clumpThickness", "uniformityOfCellSize",
                       "uniformityOfCellShape", "marginalAdhesion", "singleEpithelialCellSize",
                       "bareNuclei", "blandChromatin", "normalNucleoli", "mitoses", "class")

# check the structure of the data
str(cancerData)

# remove the id
cancerData <- select(cancerData, -id)

# check the data structure
str(cancerData)

# convert bareNuclei to number
cancerData$bareNuclei <- as.numeric(cancerData$bareNuclei)

# check the distribution to see if it needs to be normalized
summary(cancerData)

# convert class to factor
cancerData$class <- factor(ifelse(cancerData$class == 2, "benign", "malignant"))

# split observation data into training and test sets
trainPCT <- 0.7 # pct of source data frame kept in training set
sourceDF <- cancerData # source data frame that will be split into training and test
train_idx <- sample(1:nrow(sourceDF),trainPCT * nrow(sourceDF),replace=FALSE)
trainingSetFull <- sourceDF[train_idx,] # select all these rows
testSetFull <- sourceDF[-train_idx,] # select all but these rows

# remove independent variable
trainingSet <- select(trainingSetFull, -class)
testSet <- select(testSetFull, -class)

# create dependent variable df
trainingOutcomes <- select(trainingSetFull, class)
testOutcomes <- select(testSetFull, class)

# build the model
p1 <- C5.0 (trainingSet, trainingOutcomes[,1], rules = TRUE) 

# review the model output
summary(p1)

# run the model on the test data
p2 <- predict(p1, testSet)

# create the confusion matrix to check model fit
table(actuals = testOutcomes[,1], predictions = p2)
