library(e1071)
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
# train.data <- subset(train.raw, select = -remove.attributes)
train.data <- train.raw[, !(names(train.raw) %in% remove.attributes)]
train.data$Soil_Type <- soil
train.data$Wilderness_Area <- area
classtrain <- as.factor(cover)

# learn the model, type="C-classification" is set so that 
model <- svm(train.data, classtrain,type="C-classification")

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

prediction <- predict(model,test1)

results <- data.frame(Id=test$Id,Cover_Type=prediction)

write.csv(results, "svm.csv", row.names=FALSE)

