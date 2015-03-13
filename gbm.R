library(caret)
library(gbm)

dataset <- read.csv("UCIData.csv")

nbrow <- nrow(dataset)
nb <-ncol(dataset)
class <- paste("S",as.character(dataset[,nb]),sep="",collapse=NULL)

attribs <- dataset[,-nb]

train <- attribs[1:15120,]
test <- attribs[15121:nbrow,]
classtrain <- class[1:15120]
classtest <- class[15121:nbrow]

train1 <- dataset[1:15120,]
train1$Cover_Type <- as.factor(classtrain)

# Define the range of values over which we would want to cross-validate our model
Grid <-  expand.grid(
  n.trees = c(500),
  interaction.depth = c(15) ,
  shrinkage = 0.2)

# Define the parameters for cross validation
fitControl <- trainControl(method = "none", classProbs = TRUE)

# Initialize randomization seed
set.seed(1805)  

GBMmodel <- train(Cover_Type ~ .,
                  data = train1,
                  method = "gbm",
                  trControl = fitControl,
                  verbose = TRUE,
                  tuneGrid = Grid,
                  metric = "ROC")

GBMpredTrain <- predict(GBMmodel, newdata = test, type="prob")

error <- mean(GBMpredTrain != classtest)

# gbm.pred <- as.factor(colnames(GBMpredTrain)[max.col(GBMpredTrain)])
# int depth -20 -> 24.13
