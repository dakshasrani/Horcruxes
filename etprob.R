options(java.parameters = "-Xmx2048m")
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

t <- subset(train, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Wilderness_Area, Horizontal_Distance_To_Fire_Points, Horizontal_Distance_To_Hydrology, Vertical_Distance_To_Hydrology, Hillshade_9am))

et.model <- extraTrees(t, as.factor(classtrain), numRandomCuts=4, ntree=500, numThreads=3, mtry=3)


test1 <- subset(test, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Wilderness_Area, Horizontal_Distance_To_Fire_Points, Horizontal_Distance_To_Hydrology, Vertical_Distance_To_Hydrology, Hillshade_9am))

options(java.parameters = "-Xmx2048m")
library(rJava)
library(extraTrees)

et.pred <- predict(et.model, test1, probability=T)

write.csv(et.pred,"ETProb.csv",rownames=F)
