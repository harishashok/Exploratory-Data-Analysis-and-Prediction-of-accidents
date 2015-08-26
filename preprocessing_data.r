#Preprocessing of Dataset

#Day of the week chunk
accident.Data$Day_of_Week[accident.Data$Day_of_Week]<- as.character(accident.Data$Day_of_Week)
accident.Data$Day_of_Week[accident.Data$Day_of_Week == 1] = "Sunday"
accident.Data$Day_of_Week[accident.Data$Day_of_Week =="2"] = "Monday"
accident.Data$Day_of_Week[accident.Data$Day_of_Week =="3"] = "Tuesday"
accident.Data$Day_of_Week[accident.Data$Day_of_Week =="4"] = "Wednesday"
accident.Data$Day_of_Week[accident.Data$Day_of_Week =="5"] = "Thursday"
accident.Data$Day_of_Week[accident.Data$Day_of_Week =="6"] = "Friday"
accident.Data$Day_of_Week[accident.Data$Day_of_Week =="7"] = "Sunday"

#Accident Severity chunk
accident.Data$Accident_Severity <- as.character(accident.Data$Accident_Severity)
accident.Data$Accident_Severity[accident.Data$Accident_Severity == "1"]= "Fatal"
accident.Data$Accident_Severity[accident.Data$Accident_Severity == "2"]= "Serious"
accident.Data$Accident_Severity[accident.Data$Accident_Severity == "3"]= "Slight"

#Road type Chunk
accident.Data$Road_Type <- as.character(accident.Data$Road_Type)
accident.Data$Road_Type[accident.Data$Road_Type == "1"] = "Roundabout"
accident.Data$Road_Type[accident.Data$Road_Type == "2"] = "One way street"
accident.Data$Road_Type[accident.Data$Road_Type == "3"] = "Dual carriageway"
accident.Data$Road_Type[accident.Data$Road_Type == "6"] = "Single carriagew way"
accident.Data$Road_Type[accident.Data$Road_Type == "7"] = "Slip Road"
accident.Data$Road_Type[accident.Data$Road_Type == "9"] = "Two way street Slip road"
accident.Data$Road_Type[accident.Data$Road_Type == "12"] = "One way streetSlip road"
accident.Data$Road_Type[accident.Data$Road_Type == "-1"] = "Data missing or out of range"
accident.Data$Road_Type[1:5]

#Weather Condition chunk
accident.Data$Weather_Conditions <- as.character(accident.Data$Weather_Conditions)
accident.Data$Weather_Conditions[accident.Data$Weather_Conditions == "1"] = "Fine no high winds"
accident.Data$Weather_Conditions[accident.Data$Weather_Conditions == "2"] = "Raining no high winds"
accident.Data$Weather_Conditions[accident.Data$Weather_Conditions == "3"] = "Snowing no high winds"
accident.Data$Weather_Conditions[accident.Data$Weather_Conditions == "4"] = "Fine + no high winds"
accident.Data$Weather_Conditions[accident.Data$Weather_Conditions == "5"] = "Raining + high winds"
accident.Data$Weather_Conditions[accident.Data$Weather_Conditions == "6"] = "Snowing + high winds"
accident.Data$Weather_Conditions[accident.Data$Weather_Conditions == "7"] = "Fog or mist"
accident.Data$Weather_Conditions[accident.Data$Weather_Conditions == "8"] = "Other"
accident.Data$Weather_Conditions[accident.Data$Weather_Conditions == "9"] = "Unknown"
accident.Data$Weather_Conditions[accident.Data$Weather_Conditions == "-1"] = "Data missing or out of range"

#Urban or Rural chunk
accident.Data$Urban_or_Rural_Area <- as.character(accident.Data$Urban_or_Rural_Area)
accident.Data$Urban_or_Rural_Area[accident.Data$Urban_or_Rural_Area == "1"] = "Urban"
accident.Data$Urban_or_Rural_Area[accident.Data$Urban_or_Rural_Area == "2"] = "Rural"
accident.Data$Urban_or_Rural_Area[accident.Data$Urban_or_Rural_Area == "3"] = "Unallocated"
accident.Data$Urban_or_Rural_Area[1:5]

#light conditions chunk
accident.Data$Light_Conditions <- as.character(accident.Data$Light_Conditions)
accident.Data$Light_Conditions[accident.Data$Light_Conditions == "1"] = "Daylight"
accident.Data$Light_Conditions[accident.Data$Light_Conditions == "4"] = "Darkness - lights lit"
accident.Data$Light_Conditions[accident.Data$Light_Conditions == "5"] = "Darkness - lights unlit"
accident.Data$Light_Conditions[accident.Data$Light_Conditions == "6"] = "Darkness - no lighting"
accident.Data$Light_Conditions[accident.Data$Light_Conditions == "7"] = "Darkness - Dim lighting"
accident.Data$Light_Conditions[accident.Data$Data$Light_Conditions == "-1"] = "Data missing or out of range"

#Road Surface chunk
accident.Data$Road_Surface_Conditions <- as.character(accident.Data$Road_Surface_Conditions)
accident.Data$Road_Surface_Conditions[accident.Data$Road_Surface_Conditions == "1"] = "Dry"
accident.Data$Road_Surface_Conditions[accident.Data$Road_Surface_Conditions == "2"] = "Wet or damp"
accident.Data$Road_Surface_Conditions[accident.Data$Road_Surface_Conditions == "3"] = "Snow"
accident.Data$Road_Surface_Conditions[accident.Data$Road_Surface_Conditions == "4"] = "Frost or ice"
accident.Data$Road_Surface_Conditions[accident.Data$Road_Surface_Conditions == "5"] = "Flood over 3cm. deep"
accident.Data$Road_Surface_Conditions[accident.Data$Road_Surface_Conditions == "6"] = "Oil or diesel"
accident.Data$Road_Surface_Conditions[accident.Data$Road_Surface_Conditions == "7"] = "Mud"
accident.Data$Road_Surface_Conditions[accident.Data$Road_Surface_Conditions == "-1"] = "Data missing or out of range"
accident.Data$Road_Surface_Conditions[1:5]

# Converting factor to Charater
accident.Data$Time <- as.character(accident.Data$Time)

accident.Data$Time2 <- sapply(strsplit(accident.Data$Time,
":"), function(x) {
x <- as.numeric(x)
x <- x[1] + (x[2]/60)
})

#Renaming Time variables
accident.Data$TimeSegment[(accident.Data$Time2 > 4) & (accident.Data$Time2 <= 7)] = "Early Morning"
accident.Data$TimeSegment[(accident.Data$Time2 > 7) & (accident.Data$Time2 <= 11)] = "Morning"
accident.Data$TimeSegment[(accident.Data$Time2 > 11) & (accident.Data$Time2 <= 16)] = "Afternoon"
accident.Data$TimeSegment[(accident.Data$Time2 > 16) & (accident.Data$Time2 <= 20)] = "Evening"
accident.Data$TimeSegment[(accident.Data$Time2 > 20) & (accident.Data$Time2 <= 24)] = "Night"
accident.Data$TimeSegment[(accident.Data$Time2 >= 0) & (accident.Data$Time2 <= 4)] = "Late Night"
accident.Data$TimeSegment[1:5]

#Naming Hour conventions
accident.Data$Time[(accident.Data$Time2 > 0) & (accident.Data$Time2 <=1)] = "1"
accident.Data$Time[(accident.Data$Time2 > 1) & (accident.Data$Time2 <=2)] = "2"
accident.Data$Time[(accident.Data$Time2 > 2) & (accident.Data$Time2 <= 3)] = "3"
accident.Data$Time[(accident.Data$Time2 > 3) & (accident.Data$Time2 <= 4)] = "4"
accident.Data$Time[(accident.Data$Time2 > 4) & (accident.Data$Time2 <= 5)] = "5"
accident.Data$Time[(accident.Data$Time2 > 5) & (accident.Data$Time2 <= 6)] = "6"
accident.Data$Time[(accident.Data$Time2 > 6) & (accident.Data$Time2 <= 7)] = "7"
accident.Data$Time[(accident.Data$Time2 > 7) & (accident.Data$Time2 <= 8)] = "8"
accident.Data$Time[(accident.Data$Time2 > 8) & (accident.Data$Time2 <= 9)] = "9"
accident.Data$Time[(accident.Data$Time2 > 9) & (accident.Data$Time2 <= 10)] = "10"
accident.Data$Time[(accident.Data$Time2 > 10) & (accident.Data$Time2 <= 11)] = "11"
accident.Data$Time[(accident.Data$Time2 > 11) & (accident.Data$Time2 <= 12)] = "12"
accident.Data$Time[(accident.Data$Time2 > 12) & (accident.Data$Time2 <= 13)] = "13"
accident.Data$Time[(accident.Data$Time2 > 13) & (accident.Data$Time2 <= 14)] = "14"
accident.Data$Time[(accident.Data$Time2 > 14) & (accident.Data$Time2 <= 15)] = "15"
accident.Data$Time[(accident.Data$Time2 > 15) & (accident.Data$Time2 <= 16)] = "16"
accident.Data$Time[(accident.Data$Time2 > 16) & (accident.Data$Time2 <= 17)] = "17"
accident.Data$Time[(accident.Data$Time2 > 17) & (accident.Data$Time2 <= 18)] = "18"
accident.Data$Time[(accident.Data$Time2 > 18) & (accident.Data$Time2 <= 19)] = "19"
accident.Data$Time[(accident.Data$Time2 > 19) & (accident.Data$Time2 <= 20)] = "20"
accident.Data$Time[(accident.Data$Time2 > 20) & (accident.Data$Time2 <= 21)] = "21"
accident.Data$Time[(accident.Data$Time2 > 21) & (accident.Data$Time2 <= 22)] = "22"
accident.Data$Time[(accident.Data$Time2 > 22) & (accident.Data$Time2 <= 23)] = "23"
accident.Data$Time[(accident.Data$Time2 > 23) & (accident.Data$Time2 <= 24)] = "24"
