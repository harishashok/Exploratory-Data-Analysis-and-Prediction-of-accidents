#################RANDOM FORESTS##################

library(randomForest)

nrows.Train = round(0.7 * nrow(rbind.accidentData))
ndxTrain = sample(x = nrow(rbind.accidentData), size = nrows.Train)
accident.training = rbind.accidentData[ndxTrain, ]
accident.testing = rbind.accidentData[-ndxTrain, ]

accident.formula1 <- Accident_Severity ~ Day_of_Week +
Speed_limit + Road_Type + Weather_Conditions +
Light_Conditions + Urban_or_Rural_Area + Road_Surface_Conditions

accident.rf = randomForest(accident.formula1, data = accident.training, ntree = 1, proximity = TRUE)
print(accident.rf)

#We use predict command creates to generate a prediction accuracy matrix.
accident.TrainPrediction = predict(object = accident.rf, newdata = accident.training, type = "response")
accident.TestPrediction = predict(object = accident.rf, newdata = accident.testing, type = "response")

#We apply confusionMatrix command to nd out the accuracy of the random forest result.
cMatTrain = confusionMatrix(data = accident.TrainPrediction, reference = accident.training$Accident_Severity)
cMatTest = confusionMatrix(data = accident.TestPrediction, reference = accident.testing$Accident_Severity)
as.matrix(cMatTrain)
as.matrix(cMatTest)
