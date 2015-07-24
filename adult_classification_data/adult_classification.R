#http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names
source("~/Classes/DSS/Functions.R")

# check for required packages
pkg_ck(c('dplyr', 'class', 'ggvis', 'corrplot', 'gmodels', 'C50'))

setwd("~/Classes/Model_Building")

# create the target directory
trgtDir <- "adult_classification_data"

if (!file.exists(trgtDir)) {
  dir.create(trgtDir)
}

# download the data file
destFile <- "adult.data"
fileURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
download.file(fileURL, destfile = paste(trgtDir,"/",destFile, sep =""), method = "curl")
list.files(paste("./",trgtDir, sep=""))
dataDownloaded <- date()

# read in the data
adultData <- read.csv(paste(trgtDir, "/","adult.data", sep=""), header = FALSE, na.strings = c("?"))

adultNames <- c('age','workclass','fnlwgt','education','education-num','marital-status',
                'occupation','relationship','race','sex','capital-gain','capital-loss','hours-per-week',
                'native-country', 'salary_range')

# add the names to the data frame
names(adultData) <- adultNames

# check the data structure
str(adultData)

# check the data fields
summary(adultData)

# Build kNN model
## limit to continuous variables
adultDatakNN <- adultData[,c(1,5,11,12,13,15)]

## create index for random assignment to train versus test
trainPCT <- 0.7 # pct of source data frame kept in training set
sourceDF <- adultDatakNN # source data frame that will be split into training and test
train_idx <- sample(1:nrow(sourceDF),trainPCT * nrow(sourceDF),replace=FALSE)
trainingSetFull <- sourceDF[train_idx,] # select all these rows
testSetFull <- sourceDF[-train_idx,] # select all but these rows

## remove independent variable
trainingSet <- select(trainingSetFull, -salary_range)
testSet <- select(testSetFull, -salary_range)

## normalize the predictors
trainingSet <-as.data.frame(lapply(trainingSet, normalize))
testSet <-as.data.frame(lapply(testSet, normalize))

## create dependent variable df
trainingOutcomes <- select(trainingSetFull, salary_range)
testOutcomes <- select(testSetFull, salary_range)

## kNN model
k <- if((round(sqrt(nrow(trainingSet))) %% 2) > 0) {round(sqrt(nrow(trainingSet)))}else{round(sqrt(nrow(trainingSet)))+1}
knn1 <- knn(train = trainingSet, test = testSet, cl = trainingOutcomes[,1], k = k)
summary(knn1)
table(actuals = testOutcomes[,1], predictions = knn1)

# decision tree
## limit variables
adultDatac50 <- select(adultData, -fnlwgt)

## create index for random assignment to train versus test
trainPCT <- 0.7 # pct of source data frame kept in training set
sourceDF <- adultDatac50 # source data frame that will be split into training and test
train_idx <- sample(1:nrow(sourceDF),trainPCT * nrow(sourceDF),replace=FALSE)
trainingSetFull <- sourceDF[train_idx,] # select all these rows
testSetFull <- sourceDF[-train_idx,] # select all but these rows

## remove independent variable
trainingSet <- select(trainingSetFull, -salary_range)
testSet <- select(testSetFull, -salary_range)

## create dependent variable df
trainingOutcomes <- select(trainingSetFull, salary_range)
testOutcomes <- select(testSetFull, salary_range)

## decision tree model
dt <- C5.0(trainingSet, trainingOutcomes[,1])
dt2 <- predict(dt, testSet)

## run confusion matrix
table(actiuals = testOutcomes[,1], predictions = dt2)

summary(dt2)
summary(dt)
