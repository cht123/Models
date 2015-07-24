source("~/Classes/DSS/Functions.R")

# check for required packages
pkg_ck(c('dplyr', 'class', 'ggvis', 'corrplot', 'gmodels'))

setwd("~/Classes/Model_Building")


if (!file.exists("cancer_model")) {
  dir.create("Cancer_model")
}
fileURL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile = "Cancer_model/candata.csv", method = "curl")
list.files("./Cancer_model")
dataDownloaded <- date()

#read table
cancerData <- read.csv("Cancer_model/candata.csv", header = TRUE)

# add field names
names(cancerData) <- c("id", "clumpThickness", "uniformityOfCellSize",
                       "uniformityOfCellShape", "marginalAdhesion", "singleEpithelialCellSize",
                       "bareNuclei", "blandChromatin", "normalNucleoli", "mitoses", "class")

# check the structure of the data
str(cancerData)

# remove the id
cancerData <- select(cancerData, -id)

# convert bareNuclei to number
cancerData$bareNuclei <- as.numeric(cancerData$bareNuclei)

# remove all cases that have missing values
cancerData <- cancerData[complete.cases(cancerData),]

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

# create the model
k <- if((round(sqrt(nrow(trainingSet))) %% 2) > 0) {round(sqrt(nrow(trainingSet)))}else{round(sqrt(nrow(trainingSet)))+1}
predictions <- knn(train = trainingSet, test = testSet, cl = trainingOutcomes[,1], k = k)

# check misclassification rate
table(testOutcomes[,1], predictions)

# pass new cases as test set
knn(train = trainingSet, cl = trainingOutcomes, k = 21, test = newCase)

# plot values
trainingSetFull %>% ggvis(~clumpThickness, ~bareNuclei, fill = ~class) %>% layer_points()

# plot correlations
corrplot(cor(trainingSet))

CrossTable(x = testOutcomes[,1], y = predictions, prop.chisq=FALSE)
