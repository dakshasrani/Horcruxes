library(e1071)
dataset <- read.csv("UCIDataNorm.csv")

nbrow <- nrow(dataset)
nb <-ncol(dataset)
class <- as.character(dataset[,nb])

attribs <- dataset[,-nb]

ntrain <- 15121
# sample
tindex <- sample(nbrow,ntrain)
train <- attribs[tindex,]
test <- attribs[-tindex,]
classtrain <- class[tindex]
classtest <- class[-tindex]
# learn the model, type="C-classification" is set so that 
model <- svm(train, classtrain,type="C-classification")
train1 <- dataset[tindex,]
train1$Cover_Type <- as.factor(train1$Cover_Type)

nb.model <- naiveBayes(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points + Soil_Type + Wilderness_Area, data = train1)

nb.pred <- predict(nb.model, test)

error <- mean(classtest != nb.pred)