library(extraTrees)
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

t <- subset(train, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Wilderness_Area, Horizontal_Distance_To_Fire_Points, Horizontal_Distance_To_Hydrology, Vertical_Distance_To_Hydrology, Hillshade_9am))
t1 <- subset(train, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Wilderness_Area, Horizontal_Distance_To_Fire_Points, Horizontal_Distance_To_Hydrology, Vertical_Distance_To_Hydrology,EVDtH,EHDtH))
t2 <- subset(train, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Wilderness_Area, 
                               Horizontal_Distance_To_Fire_Points, 
                               Horizontal_Distance_To_Hydrology, Hillshade_3pm, Hillshade_9am))

# et.model <- extraTrees(train, as.factor(classtrain), numRandomCuts=4, ntree=500, numThreads=2, mtry=5)

et.model <- extraTrees(t, as.factor(classtrain), numRandomCuts=4, ntree=500, numThreads=3, mtry=3)


test1 <- subset(test, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Wilderness_Area, Horizontal_Distance_To_Fire_Points, Horizontal_Distance_To_Hydrology, Vertical_Distance_To_Hydrology, Hillshade_9am))
test2 <- subset(test, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Wilderness_Area, Horizontal_Distance_To_Fire_Points, Horizontal_Distance_To_Hydrology, Vertical_Distance_To_Hydrology,EVDtH,EHDtH))
test3 <- subset(test, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Wilderness_Area, 
                               Horizontal_Distance_To_Fire_Points, 
                               Horizontal_Distance_To_Hydrology, Hillshade_3pm, Hillshade_9am))

options(java.parameters = "-Xmx2048m")
library(rJava)
library(extraTrees)

et.pred <- predict(et.model, test1)

# et.pred <- predict(et.model, test)

et.error <- mean(et.pred != classtest)
# --------------------------------------------------------------------------------
t1 <- train
t1$dth <- sqrt(t1$Horizontal_Distance_To_Hydrology^2 + t1$Vertical_Distance_To_Hydrology^2)

t1 <- subset(t1,select=-c(Horizontal_Distance_To_Hydrology,Vertical_Distance_To_Hydrology))

et.model1 <- extraTrees(t1, as.factor(classtrain), numRandomCuts=4, ntree=500, numThreads=2, mtry=5)

options( java.parameters = "-Xmx1g" )
library(extraTrees)

test2.1 <- test
test2.1$dth <- sqrt(test2.1$Horizontal_Distance_To_Hydrology^2 + test2.1$Vertical_Distance_To_Hydrology^2)

test2.1 <- subset(test2.1,select=-c(Horizontal_Distance_To_Hydrology,Vertical_Distance_To_Hydrology))

et.pred1 <- predict(et.model1, test2.1)
et.error1 <- mean(et.pred1 != classtest)

# --------------------------------------------------------------------------------

train$EVDtH <- train$Elevation-train$Vertical_Distance_To_Hydrology
test$EVDtH = test$Elevation-test$Vertical_Distance_To_Hydrology

train$EHDtH = train$Elevation-train$Horizontal_Distance_To_Hydrology*0.2
test$EHDtH = test$Elevation-test$Horizontal_Distance_To_Hydrology*0.2

et.model2 <- extraTrees(train, as.factor(classtrain), numRandomCuts=4, ntree=500, numThreads=2, mtry=5)

options( java.parameters = "-Xmx1g" )
# setJavaMemory(1500)
library(extraTrees)

et.pred2 <- predict(et.model2, test)
et.error2 <- mean(et.pred2 != classtest)

# ----------------------------------------------------------------------------------
# Feature Engineering

train$EVDtH <- train$V1-train$V4
test$EVDtH = test$V1-test$V4

train$EHDtH = train$V1-train$V5*0.2
test$EHDtH = test$V1-test$V5*0.2

et.model2 <- extraTrees(train, as.factor(classtrain), ntree=1000, numThreads=5, mtry=10)

options( java.parameters = "-Xmx2g" )
# setJavaMemory(1500)
library(extraTrees)

et.pred2 <- predict(et.model2, test)
et.error2 <- mean(et.pred2 != classtest)