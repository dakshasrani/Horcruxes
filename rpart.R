library(rpart)
library(RCurl)
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
train.data <- train.raw[, !(names(train.raw) %in% remove.attributes)]
train.data$Soil_Type <- soil
train.data$Wilderness_Area <- area
train.data$Cover_Type <- as.factor(cover)


#model with features 1,5,6,10,11,12
rpart.model <- rpart(as.character(Cover_Type) ~ Elevation + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways + Horizontal_Distance_To_Fire_Points + Soil_Type + Wilderness_Area, data = train.data)

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

rpart.pred  <- predict(rpart.model, test1)

result.class <- colnames(rpart.pred)[apply(rpart.pred,1,which.max)]

results <- data.frame(Id=test$Id,Cover_Type=result.class)

write.csv(results, "rpart.csv", row.names=FALSE)
