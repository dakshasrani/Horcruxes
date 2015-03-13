library(glmnet)

dataset <- read.csv("UCIDataNorm.csv")

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

glmnet.model <- glmnet(as.matrix(train),as.factor(classtrain), family="multinomial")

test1 <- test[1:40,]
glmnet.pred <- predict(glmnet.model, as.matrix(test1), family="probs")

pred <- as.factor(colnames(glmnet.pred)[max.col(glmnet.pred)])

result.class <- colnames(glmnet.pred)[apply(glmnet.pred,1,which.max)]

preds <- data.frame(lapply(glmnet.pred, function(x) x[,2]))

out <- apply(glmnet.pred, 1, function(x) names(glmnet.pred)[which.max(x)])
