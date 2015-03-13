library(RCurl)
train.data.url <- getURL("https://raw.githubusercontent.com/dakshasrani/HorcruxesW15/master/data/TrainData.csv")
train.data <- read.csv(text = train.data.url)

uci.data <- read.csv("UCIData.csv")

test.data.url <- getURL("https://raw.githubusercontent.com/dakshasrani/HorcruxesW15/master/data/TestData.csv")
test.data <- read.csv(text = test.data.url)

normalize <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))} 

train.data.norm <- as.data.frame(apply(train.data, MARGIN = 2, normalize))
test.data.norm <- as.data.frame(apply(test.data, MARGIN = 2, normalize))

uci.data.norm <- as.data.frame(apply(uci.data, MARGIN = 2, normalize))

write.csv(train.data.norm, "TrainDataNorm.csv",row.names=FALSE)
write.csv(test.data.norm, "TestDataNorm.csv",row.names=FALSE)

write.csv(uci.data.norm, "UCIDataNorm.csv", row.names=FALSE)
