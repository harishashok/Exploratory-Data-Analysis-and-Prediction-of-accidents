#######DATA ANALYSIS SECTION#########
We apply different analytical techniques to determine the factors affecting
the severity of accidents happening in UK from 2005 to 2012. In addition,
we identify whether accidents are occurring in rural or urban area. These
analysis can be used by the UK police department to control the accident
severity.
All vectors(collection) of data in the data frame are converted to factor fo
analysis as follows.

accident.Data$Speed_limit = as.factor(accident.Data$Speed_limit)
accident.Data$Weather_Conditions = as.factor(accident.Data$Weather_Conditions)
accident.Data$Road_Surface_Conditions = as.factor(accident.Data$Road_Surface_Conditions)
accident.Data$Day_of_Week = as.factor(accident.Data$Day_of_Week)
accident.Data$Road_Type = as.factor(accident.Data$Road_Type)
accident.Data$Accident_Severity = as.factor(accident.Data$Accident_Severity)
accident.Data$Urban_or_Rural_Area = as.factor(accident.Data$Urban_or_Rural_Area)
accident.Data$Number_of_Casualties = as.factor(accident.Data$Number_of_Casualties)
accident.Data$Time = as.factor(accident.Data$Time)
accident.Data$Light_Conditions <- as.factor(accident.Data$Light_Conditions)


library(ggplot2)
# Extracting the year from the Accident Index
accident.Data$Year = substring(accident.Data$Accident_Index,
1, 4)
# Count of Accidents with respect to day of the
# week and year
qplot(data = accident.Data, x = Day_of_Week, fill = Year)

newFrame = subset(accident.Data, Day_of_Week == "Friday",
select = c(Accident_Index, Year, Day_of_Week, Time,
Time2, TimeSegment))
qplot(data = newFrame, x = Time, fill = Year)

qplot(data = accident.Data, x = Day_of_Week, fill = Time)
