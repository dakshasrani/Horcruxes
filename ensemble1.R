library(RCurl)
library(randomForest)
train.data.url <- getURL("https://raw.githubusercontent.com/dakshasrani/Horcruxes/master/data/train.csv")
train.raw <- read.csv(text = train.data.url)

soils <- paste("Soil_Type", sep = "", collapse = NULL, 1:40)
areas <- paste("Wilderness_Area", sep = "", collapse = NULL, 1:4)

df = data.frame(train.raw[soils])
df$soil = 0
for(colname in soils)({
  coldata=df[,colname]
  df$soil[which(coldata==1)]=colname
})
df$soil=gsub("Soil_Type","",df$soil,fixed=T)
soil <- as.numeric(df$soil)

df = data.frame(train.raw[areas])
df$area = 0
for(colname in areas)({
  coldata=df[,colname]
  df$area[which(coldata==1)]=colname
})
df$area=gsub("Wilderness_Area","",df$area,fixed=T)
area <- as.numeric(df$area)

# create the train data with one soil and one cover
cover <- train.raw$Cover_Type

remove.attributes <- c('Id',soils, areas, 'Cover_Type')
# train.data <- subset(train.raw, select = -remove.attributes)
train.data <- train.raw[, !(names(train.raw) %in% remove.attributes)]
train.data$Soil_Type <- soil
train.data$Wilderness_Area <- area
train.data$Cover_Type <- as.factor(paste('S',as.character(cover),sep="",collapse=NULL))

rf.model <- randomForest(Cover_Type ~ Elevation + Horizontal_Distance_To_Hydrology + Horizontal_Distance_To_Roadways + Horizontal_Distance_To_Fire_Points + Soil_Type, data = train.data, importance=TRUE, ntree=500, mtry = 4)

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
                  data = train.data,
                  method = "gbm",
                  trControl = fitControl,
                  verbose = TRUE,
                  tuneGrid = Grid,
                  metric = "ROC")


test.data.url <- getURL("https://raw.githubusercontent.com/dakshasrani/Horcruxes/master/data/test.csv")
test <- read.csv(text = test.data.url)

df = data.frame(test[soils])
df$soil = 0
for(colname in soils)({
  coldata=df[,colname]
  df$soil[which(coldata==1)]=colname
})
df$soil=gsub("Soil_Type","",df$soil,fixed=T)
soil <- as.numeric(df$soil)

df = data.frame(test[areas])
df$area = 0
for(colname in areas)({
  coldata=df[,colname]
  df$area[which(coldata==1)]=colname
})
df$area=gsub("Wilderness_Area","",df$area,fixed=T)
area <- as.numeric(df$area)

remove.attributes <- c('Id',soils, areas)

test1 <- test[, !(names(test) %in% remove.attributes)]
test1$Soil_Type <- soil
test1$Wilderness_Area <- area

rf.pred  <- predict(rf.model, test1, type="prob")
GBMpredTrain <- predict(GBMmodel, newdata = test1, type="prob")

probs <- 0.9*rf.pred + 0.1*GBMpredTrain
final.pred <- as.factor(colnames(probs)[max.col(probs)])

class <- as.character(final.pred)
class <- as.numeric(substr(class,2,2))

results <- data.frame(Id=test$Id,Cover_Type=class)


write.csv(results, "ensembleRFGBM.csv", row.names=FALSE)
