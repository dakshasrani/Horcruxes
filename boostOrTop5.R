et.pred.train <- predict(et.model,testA5)

rf.pred.train <- predict(rf.model2,test1)

weights <- rep(1, 15120)
S2.rows <- which(classtrain == 'S2')
S1.rows <- which(classtrain == 'S1')
weights[S2.rows] <- 2
weights[S1.rows] <- 2

et.model <- extraTrees(t, as.factor(classtrain), numRandomCuts=4, ntree=300, 
                       numThreads=3, mtry=5, weights=weights)

tA5 <- subset(train, select=c(Elevation, Vertical_Distance_To_Hydrology,Horizontal_Distance_To_Roadways, Horizontal_Distance_To_Fire_Points, Soil_Type, Wilderness_Area))

testA5 <- subset(test, select=c(Elevation, Vertical_Distance_To_Hydrology,Horizontal_Distance_To_Roadways, Horizontal_Distance_To_Fire_Points, Soil_Type, Wilderness_Area))
et.model <- extraTrees(tA5, as.factor(classtrain), numRandomCuts=4, ntree=500, numThreads=3, mtry=5)
