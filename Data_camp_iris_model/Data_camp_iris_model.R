
source("~/Classes/DSS/Functions.R")

# check for required packages
pkg_ck(c('dplyr', 'class', 'ggvis', 'corrplot', 'gmodels'))

# change the working directory
setwd("~/Classes/Model_Building")

# target directory for the downloaded file 
trgtDir <- "Data_camp_iris_model"

# target name for the downloaded file
trgtFile <- "iris"

# create the directory if it doesnt exist
if (!file.exists(trgtDir)) {
  dir.create(trgtDir)
}

# url of the file being downloaded
fileURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"

# get the file
download.file(fileURL, destfile = paste(trgtDir,"/",trgtFile,".csv", sep = ""), method = "curl")
list.files(paste("./",trgtDir, sep = ""))

# read the file
iris <- read.csv(paste(trgtDir,"/",trgtFile,".csv", sep = ""), header = FALSE)

# add names to the frame
names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

## Investigate data
# create scatterplot
iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points()

head(iris)
str(iris)

table(iris$Species)

round(prop.table(table(iris$Species)) * 100, digits = 1)
summary(iris)

#normalize the data set
irisdata <- as.data.frame(lapply(iris[,1:4], normalize))
iris <- cbind(irisdata, iris$Species)

# create training and test set
set.seed(1234)

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

iris.training <- iris[ind==1, 1:4]
iris.test <- iris[ind==2, 1:4]

iris.trainLabels <- iris[ind==1, 5]
iris.testLabels <- iris[ind==2, 5]

iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)

# evaluate results
cbind(iris_pred, iris.testLabels)

CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)
