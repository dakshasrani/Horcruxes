library(RCurl)
train.data.url <- getURL("https://raw.githubusercontent.com/dakshasrani/HorcruxesW15/master/data/train.csv")
train.raw <- read.csv(text = train.data.url)

# soil 1-40 and cover 1-4 into soil and cover
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
train.data$Cover_Type <- cover

write.csv(train.data, "TrainData.csv",row.names=FALSE)