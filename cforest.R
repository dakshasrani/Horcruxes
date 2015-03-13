install.packages("party")
library(party)

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

library(randomForest)

set.seed(123)

train1 <- dataset[tindex,]
train1$Cover_Type <- as.factor(train1$Cover_Type)

cf.model <- cforest(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points + Soil_Type + Wilderness_Area, data = train1, controls=cforest_unbiased(ntree=300, mtry=3))

t <- subset(train1, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Wilderness_Area, Horizontal_Distance_To_Fire_Points, Horizontal_Distance_To_Hydrology, Vertical_Distance_To_Hydrology, Hillshade_9am, Cover_Type))

cf.model <- cforest(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points + Soil_Type + Wilderness_Area, data = train1, controls=cforest_unbiased(ntree=300, mtry=3))

cf.pred  <- predict(cf.model, test, type="response", OOB=TRUE)

cf.pred  <- predict(cf.model, test[1:1000,], type="response", OOB=FALSE)
# 32.7

cf.pred  <- predict(cf.model, test[1:1000,], type="response", OOB=TRUE)
# 32.7

error <- mean(classtest[1:1000] != cf.pred)

cf.model2 <- cforest(Cover_Type ~ . , data=t, controls=cforest_unbiased(ntree=300, mtry=3))

cf.pred2 <- predict(cf.model2, test[1:1000,], type="response", OOB=TRUE)

error <- mean(classtest[1:1000] != cf.pred2)

# 31.9 with t