cn <- colnames(dataset)
colnames(covtype) <- cn[2:56]
dataset <- covtype
# soil 1-40 and cover 1-4 into soil and cover
soils <- paste("Soil_Type", sep = "", collapse = NULL, 1:40)
areas <- paste("Wilderness_Area", sep = "", collapse = NULL, 1:4)

df = data.frame(covtype[soils])
df$soil = 0
for(colname in soils)({
  coldata=df[,colname]
  df$soil[which(coldata==1)]=colname
})
df$soil=gsub("Soil_Type","",df$soil,fixed=T)
soil <- as.numeric(df$soil)

df = data.frame(covtype[areas])
df$area = 0
for(colname in areas)({
  coldata=df[,colname]
  df$area[which(coldata==1)]=colname
})
df$area=gsub("Wilderness_Area","",df$area,fixed=T)
area <- as.numeric(df$area)

# create the train data with one soil and one cover
cover <- covtype$Cover_Type

remove.attributes <- c(soils, areas, 'Cover_Type')
# train.data <- subset(covtype, select = -remove.attributes)
train.data <- covtype[, !(names(covtype) %in% remove.attributes)]
train.data$Soil_Type <- soil
train.data$Wilderness_Area <- area
train.data$Cover_Type <- cover

write.csv(train.data, "UCIData.csv",row.names=FALSE)