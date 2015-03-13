install.packages("randomForest")

dataset <- read.csv("UCIData.csv")

nbrow <- nrow(dataset)
nb <-ncol(dataset)
class <- as.character(dataset[,nb])

attribs <- dataset[,-nb]

ntrain <- 15121
# sample
train <- attribs[1:15120,]
test <- attribs[15121:nbrow,]
classtrain <- class[1:15120]
classtest <- class[15121:nbrow]

library(randomForest)

set.seed(123)

train1 <- dataset[1:15120,]
train1$Cover_Type <- as.factor(train1$Cover_Type)

rf.model <- randomForest(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points + Soil_Type + Wilderness_Area, data = train1, importance=TRUE, ntree=300)

rf.pred  <- predict(rf.model, test, type="response")

error <- mean(classtest != rf.pred)

varImpPlot(rf.model2)

t <- subset(train1, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Wilderness_Area, Horizontal_Distance_To_Fire_Points, Horizontal_Distance_To_Hydrology, Vertical_Distance_To_Hydrology, Hillshade_9am, Cover_Type))
t1 <- subset(train1, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Horizontal_Distance_To_Fire_Points, Horizontal_Distance_To_Hydrology, Vertical_Distance_To_Hydrology, Aspect, Cover_Type))
t2 <- subset(train1, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Horizontal_Distance_To_Fire_Points, Hillshade_9am, Cover_Type))

rf.model2 <- randomForest(Cover_Type ~ . , data=t, importance=TRUE, mtry=3, ntrees=500)

test1 <- subset(test, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Wilderness_Area, Horizontal_Distance_To_Fire_Points, Horizontal_Distance_To_Hydrology, Vertical_Distance_To_Hydrology, Hillshade_9am))

rf.pred2 <- predict(rf.model2, test1, type="response")

error <- mean(classtest != rf.pred2)

# With all columns - 16. with raw data - 24.7
# with t - 15.5, with raw data - 23.63
