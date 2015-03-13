library(bst)

dataset <- read.csv("UCIData.csv")

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

model <- bst(train, as.numeric(classtrain), cost=0.5, family="gaussian", ctrl=bst_control())

prec <- predict(model, test, type="response")

classnum <- as.numeric(classtest)
error <- mean(classnum-prec)
# prec values between 0.1 and 0.2
# no point

train1 <- dataset[tindex,]
train1$Cover_Type <- as.factor(train1$Cover_Type)

ctrl <- trainControl(method = "repeatedcv",repeats = 3, classProbs = TRUE)

plsFit <- train(Cover_Type ~ ., data = train1,method = "pls",tuneLength = 15,trControl = ctrl,preProc = c("center", "scale"))

predFit <- predict(plsFit, test)

errorPls <- mean(classtest!=predFit)

# error - 0.3674. with raw data - 0.369
