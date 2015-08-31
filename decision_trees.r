#######################DECISION TREES#######################
We use decsion tree that uses tree like graphs to nd out factors leading to
(Accident Severity).
We use the following libraries to create decision trees.
library(rpart)
library(rpart.plot)
library(maptree)
library(rattle)
library(RColorBrewer)
library(party)
library(caret)
library(e1071)

accident.decision.data <- subset(accident.Data, Road_Type != "Unknown", select = c("Day_of_Week", "Road_Surface_Conditions",
"Weather_Conditions", "Road_Type", "Urban_or_Rural_Area", "Accident_Severity", "Time", "Light_Conditions", "Number_of_Casualties", "Speed_limit"))

# str(accident.final.data$Road_Type)

set.seed(12345)
fatal.decision.data = subset(accident.decision.data, (Accident_Severity == "Fatal"))
nonfatal.decision.data = subset(accident.decision.data, (Accident_Severity == "Non Fatal"))

set.seed(12345)
fatal.decision.data = fatal.decision.data[sample(nrow(fatal.decision.data),10000), ]
nonfatal.decision.data = nonfatal.decision.data[sample(nrow(nonfatal.decision.data),
10000), ] 

accident.formula <- Accident_Severity ~ Number_of_Casualties + Day_of_Week + Speed_limit + Road_Type + Weather_Conditions +
Light_Conditions + Urban_or_Rural_Area + Road_Surface_Conditions

rbind.accidentData <- rbind(fatal.decision.data, nonfatal.decision.data)

#####Decision Tree (rpart)######
#We run below rpart function to analyze and plot the results. 
accident.severity.rpart = rpart(data = rbind.accidentData, formula = accident.formula, )
#accident.severity.rpart

draw.tree(accident.severity.rpart, nodeinfo = TRUE)

#####Decision Tree (ctree)######
accident.control = ctree_control(maxdepth = 3)
accident.ctree = ctree(data = rbind.accidentData, formula = accident.formula, control = accident.control)

plot(accident.ctree)
