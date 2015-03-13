library(RCurl)
test.data.url <- getURL("https://raw.githubusercontent.com/dakshasrani/HorcruxesW15/master/data/test.csv")
test.raw <- read.csv(text = test.data.url)

# soil 1-40 and cover 1-4 into soil and cover
soils <- paste("Soil_Type", sep = "", collapse = NULL, 1:40)
areas <- paste("Wilderness_Area", sep = "", collapse = NULL, 1:4)

df = data.frame(test.raw[soils])
df$soil = 0
for(colname in soils)({
  coldata=df[,colname]
  df$soil[which(coldata==1)]=colname
})
df$soil=gsub("Soil_Type","",df$soil,fixed=T)
soil <- as.numeric(df$soil)

df = data.frame(test.raw[areas])
df$area = 0
for(colname in areas)({
  coldata=df[,colname]
  df$area[which(coldata==1)]=colname
})
df$area=gsub("Wilderness_Area","",df$area,fixed=T)
area <- as.numeric(df$area)

# create the test data with one soil and one cover
cover <- test.raw$Cover_Type

remove.attributes <- c('Id',soils, areas)
# test.data <- subset(test.raw, select = -remove.attributes)
test.data <- test.raw[, !(names(test.raw) %in% remove.attributes)]
test.data$Soil_Type <- soil
test.data$Wilderness_Area <- area
# test.data$Cover_Type <- cover

write.csv(test.data, "TestData.csv",row.names=FALSE)