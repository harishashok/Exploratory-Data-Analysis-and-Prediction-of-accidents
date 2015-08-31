#Association rules
#We apply association rule for discovering interesting relations between the variables that would determine the factors which aect the severity of 
accidents. This information can be used as a basis for decisions about accident severity and the area where maximum accidents occur.
The final dataset contains the metioned columns in the code and filter the unknown data value for Road Type attribute.

accident.final.data <- subset(accident.Data, Road_Type != "Unknown", select = c("Day_of_Week", "Road_Surface_Conditions",
"Weather_Conditions", "Road_Type", "Urban_or_Rural_Area", "Accident_Severity", "Time", "Light_Conditions"))

#########Association rules for accident severity########
library(arules)
library(arulesViz)

set.seed(12345)
fatal.data = subset(accident.final.data, (Accident_Severity == "Fatal"))
nonFatal.data = subset(accident.final.data, (Accident_Severity == "Non Fatal"))

set.seed(12345)
rbind.fatal.data = fatal.data[sample(nrow(fatal.data), 20000), ]
rbind.nonfatal.data = nonFatal.data[sample(nrow(nonFatal.data), 20000), ]

######Defining target variable#######
apriori1.appearance = list(rhs = c("Accident_Severity=Fatal"), default = "lhs")
apriori2.appearance = list(rhs = c("Accident_Severity=Non Fatal"), default = "lhs")
apriori1.parameter = list(minlen = 4, supp = 0.005, conf = 0.8)
apriori.control = list(verbose = F) 

#####Applying 'apriori()' algorithm for association rules#######
accident.severity.fatal = apriori(rbind.fatal.data, parameter = apriori1.parameter, appearance = apriori1.appearance,control = apriori.control)
accident.severity.nonfatal = apriori(rbind.nonfatal.data, parameter = apriori1.parameter, appearance = apriori2.appearance, control = apriori.control)

fatalrules.sorted.by.lift = sort(accident.severity.fatal, by = "lift", decreasing = TRUE)

#summary(accident.severity.fatal)
#inspect(fatalrules.sorted.by.lift[1:5])

The inspection of data with non fatal accident severity is as follows. nonFatalrules.sorted.by.lift = sort(accident.severity.nonfatal,
by = "lift", decreasing = TRUE)

summary(accident.severity.nonfatal)

inspect(nonFatalrules.sorted.by.lift[1:5])


######Applying Association rules for predicting Urban and Rural#########
set.seed(12345)
urban.data = subset(accident.final.data, (Urban_or_Rural_Area == "Urban"))
rural.data = subset(accident.final.data, (Urban_or_Rural_Area == "Rural"))

set.seed(12345)
rbind.urban.data = urban.data[sample(nrow(urban.data), 10000), ]
rbind.rural.data = rural.data[sample(nrow(rural.data), 10000), ]


Dening target variable

apriori3.appearance = list(rhs = c("Urban_or_Rural_Area=Urban"), default = "lhs")
apriori4.appearance = list(rhs = c("Urban_or_Rural_Area=Rural"), default = "lhs")

#########Applying Association rules #########
#The following command applies the apriori algorithm to find the rules having accidents in urban area.

accident.urban = apriori(rbind.urban.data, appearance = apriori3.appearance, parameter = apriori1.parameter, control = apriori.control)

#The following command applies the apriori algorithm to nd the rules having accidents in rural area.

accident.rural = apriori(rbind.rural.data, parameter = apriori1.parameter, appearance = apriori4.appearance, control = apriori.control)


urbanRrules.sorted.by.lift = sort(accident.urban, by = "lift", decreasing = TRUE)

inspect(urbanRrules.sorted.by.lift[1:5])

#summary(accident.rural)

ruralRrules.sorted.by.lift = sort(accident.rural, by = "lift", decreasing = TRUE)
inspect(ruralRrules.sorted.by.lift[1:5])
