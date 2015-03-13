oneVsAll <- function(X,Y,FUN,...) {
  models <- lapply(unique(Y), function(x) {
    name <- as.character(x)
    .Target <- factor(ifelse(Y==name,name,'other'), levels=c(name, 'other'))
    dat <- data.frame(.Target, X)
    model <- FUN(.Target~., data=dat, ...)
    return(model)
  })
  names(models) <- unique(Y)
  info <- list(X=X, Y=Y, classes=unique(Y))
  out <- list(models=models, info=info)
  class(out) <- 'oneVsAll'
  return(out)
}

predict.oneVsAll <- function(object, newX=object$info$X, ...) {
  stopifnot(class(object)=='oneVsAll')
  lapply(object$models, function(x) {
    predict(x, newX, ...)
  })
}

classify <- function(dat) {
  out <- dat/rowSums(dat)
  out$Class <- apply(dat, 1, function(x) names(dat)[which.max(x)])
  out
}

library(ada)
library(caret) 
X <- iris[,-5]
Y <- iris[,5]
myModels <- oneVsAll(train, classtrain, ada)
preds <- predict(myModels, test[1:10000,], type='probs')
preds <- data.frame(lapply(preds, function(x) x[,2])) #Make a data.frame of probs
preds <- classify(preds)

class <- paste('X',classtest[1:20000],sep="",collapse=NULL)

mean(class!=preds$Class)

library(MASS)
myModels <- oneVsAll(train, classtrain, lda)
preds <- predict(myModels, test[1:20000,])
preds <- data.frame(lapply(preds, function(x) x[[2]][,1])) #Make a data.frame of probs
preds <- classify(preds)