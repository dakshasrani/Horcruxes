install.packages("randomForest")
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

library(randomForest)

set.seed(123)

rf.model <- randomForest(Cover_Type ~ ., data = train1, importance=TRUE, ntree=300)

# rf.pred  <- predict(rf.model, test, type="response")
rf.pred  <- predict(rf.model, test, type="prob")

error <- mean(classtest != rf.pred)

varImpPlot(rf.model)

t <- subset(train1, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Wilderness_Area, Horizontal_Distance_To_Fire_Points, Horizontal_Distance_To_Hydrology, Vertical_Distance_To_Hydrology, Hillshade_9am, Cover_Type))
t1 <- subset(train1, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Horizontal_Distance_To_Fire_Points, Horizontal_Distance_To_Hydrology, Vertical_Distance_To_Hydrology, Aspect, Cover_Type))

rf.model2 <- randomForest(Cover_Type ~ . , data=t, importance=TRUE, mtry=3, ntree=500)

# rf.pred2 <- predict(rf.model2, test, type="response")
rf.pred2 <- predict(rf.model2, test, type="prob")

error <- mean(classtest != rf.pred2)

# With all columns - 16. with raw data - 16.78
# with t - 15.5, with raw data - 15.49
# with t1 - 16.5, raw - 16.16