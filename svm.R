library(e1071)
dataset <- read.csv("UCIData.csv")

nbrow <- nrow(dataset)
nb <-ncol(dataset)
class <- as.character(dataset[,nb])

attribs <- dataset[,-nb]

train <- attribs[1:15120,]
test <- attribs[15121:nbrow,]
classtrain <- class[1:15120]
classtest <- class[15121:nbrow]

# learn the model, type="C-classification" is set so that 
model <- svm(train, classtrain,type="C-classification")
prediction <- predict(model,test)

train1 <- dataset[1:15120,]
train1$Cover_Type <- as.factor(train1$Cover_Type)

svm.model <- svm(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points + Soil_Type + Wilderness_Area, data = train1, type="C-classification")

svm.pred <- predict(svm.model, test)

# confusion matrix
tab <- table(pred = prediction, true = classtest)

error1 <- mean(classtest != prediction) # 36.37

error2 <- mean(classtest != svm.pred)

# obj <- tune.svm(train, as.factor(classtrain), type="C-classification", gamma = seq(.5, 1, by = .1), cost = seq(100,1000, by = 100))