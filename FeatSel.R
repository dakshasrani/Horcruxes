library(CORElearn)

estReliefF <- attrEval(Cover_Type ~ ., train1, estimator="DistAUC")


Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Wilderness_Area, 
Horizontal_Distance_To_Fire_Points, 
Horizontal_Distance_To_Hydrology, Hillshade_3pm, Hillshade_9am, Cover_Type))

test1 <- subset(test, select=c(Elevation, Soil_Type, Horizontal_Distance_To_Roadways, Wilderness_Area, 
                            Horizontal_Distance_To_Fire_Points, 
                            Horizontal_Distance_To_Hydrology, Hillshade_3pm, Hillshade_9am))