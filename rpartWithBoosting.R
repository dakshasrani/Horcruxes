install.packages("rpart")
require(rpart)
library(rpart)

install.packages("adabag")
library(adabag)

inputData <- read.csv("UCIDataNorm.csv")

smp_size <- 15121

## set the seed to make your partition reproductible
set.seed(123)

train_ind <- sample(seq_len(nrow(inputData)), size = smp_size)

train <- inputData[train_ind, ]
test <- inputData[-train_ind, ]

rpart.model <- rpart(as.character(Cover_Type) ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points + Soil_Type + Wilderness_Area, data = train)
rpart.pred  <- predict(rpart.model, test)

result.class <- colnames(rpart.pred)[apply(rpart.pred,1,which.max)]

error <- mean(result.class != test[,13])

#plot(rpart.model)
#text(rpart.model, use.n = TRUE, all = TRUE, cex = 0.8)

#type.adaboost <- boosting(as.character(Cover_Type) ~ factor(train$Elevation) + factor(train$Aspect) + factor(train$Slope) + factor(train$Horizontal_Distance_To_Hydrology) + factor(train$Vertical_Distance_To_Hydrology) + factor(train$Horizontal_Distance_To_Roadways) + factor(train$Hillshade_9am) + factor(train$Hillshade_Noon) + factor(train$Hillshade_3pm) + factor(train$Horizontal_Distance_To_Fire_Points) + factor(train$Soil_Type) + factor(train$Wilderness_Area), data = train)
#type.adaboost <- boosting(as.character(train$Cover_Type) ~ train$Elevation + train$Aspect + train$Slope + train$Horizontal_Distance_To_Hydrology + train$Vertical_Distance_To_Hydrology + train$Horizontal_Distance_To_Roadways + train$Hillshade_9am) + factor(train$Hillshade_Noon + train$Hillshade_3pm + train$Horizontal_Distance_To_Fire_Points + train$Soil_Type + train$Wilderness_Area, data = train)
train$Cover_Type <- factor(train$Cover_Type)
#train$Soil_Type <- factor(train$Soil_Type)
#train$Wilderness_Area <- factor(train$Wilderness_Area)

type.adaboost <- boosting(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points + Soil_Type + Wilderness_Area, data = train)
test <- test[,-13]
test$Cover_Type <- as.factor(test$Cover_Type)
type.adaboost.pred <- predict.boosting(type.adaboost,newdata=test)
type.adaboost.pred$confusion

type.adaboost.pred$error

# 0.2852
