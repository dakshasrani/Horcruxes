probs <- 0.95*rf.pred2 + 0.05*GBMpredTrain
final.pred <- as.factor(colnames(probs)[max.col(probs)])
ensemble2.error <- mean(final.pred!=classtest)

gbm.class <-  as.factor(colnames(GBMpredTrain)[max.col(GBMpredTrain)])
rf.class <-  as.factor(colnames(rf.pred)[max.col(rf.pred)])