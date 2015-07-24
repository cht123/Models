pkg_ck(c('C50'))

# get the data
data("mtcars")

# check the structure
str(mtcars)

# limit to data for model
modelData <- mtcars[,1:4]

# convert cylinder to factor for classification
modelData[,2] <- factor(modelData[,2])

# look at distribution of data
summary(modelData[c(1,3,4)])
## the ranges are way off so the model could be too heavily influenced by disp and hp, need to normalize

## create index for random assignment to train versus test
trainPCT <- 0.5 # pct of source data frame kept in training set
sourceDF <- modelData # source data frame that will be split into training and test
train_idx <- sample(1:nrow(sourceDF),trainPCT * nrow(sourceDF),replace=FALSE)
trainingSetFull <- sourceDF[train_idx,] # select all these rows
testSetFull <- sourceDF[-train_idx,] # select all but these rows

# remove independent variable
trainingSet <- select(trainingSetFull, -cyl)
testSet <- select(testSetFull, -cyl)

# normalize the predictors
trainingSet <-as.data.frame(lapply(trainingSet, normalize))
testSet <-as.data.frame(lapply(testSet, normalize))

# create dependent variable df
trainingOutcomes <- select(trainingSetFull, cyl)
testOutcomes <- select(testSetFull, cyl)

# kNN model
k <- if((round(sqrt(nrow(trainingSet))) %% 2) > 0) {round(sqrt(nrow(trainingSet)))}else{round(sqrt(nrow(trainingSet)))+1}
knn1 <- knn(train = trainingSet, test = testSet, cl = trainingOutcomes[,1], k = k)

table(actuals = testOutcomes[,1], predictions = knn1)

# decision tree model
dt <- C5.0(trainingSet, trainingOutcomes[,1])
dt2 <- predict(dt, testSet)

# run confusion matrix
table(actiuals = testOutcomes[,1], predictions = dt2)

summary(dt2)
summary(dt)
