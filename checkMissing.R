install.packages("Amelia")
require(Amelia)
train.data.url <- getURL("https://raw.githubusercontent.com/dakshasrani/HorcruxesW15/master/data/TrainData.csv")
df.train <- read.csv(text = train.data.url)
missmap(df.train, main="Forest Cover Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)