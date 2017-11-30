setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
monthList <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
locList <- c("Vancouver", "Kelowna", "Calgary")
DataAll <- read.csv("all_CLEANED.csv", header = TRUE)
DataAll$Date.Time <- as.Date(DataAll$Date.Time)
DataAll$Month <- factor(DataAll$Month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = monthList);
DataAll$City <- factor(DataAll$City, levels = c(1,2,3), labels = locList);



dfKel <- subset(DataAll, City == "Kelowna" & Year >=2006)
dfVan <- subset(DataAll, City == "Vancouver"& Year >=2006)
dfCal <- subset(DataAll, City == "Calgary"& Year >=2006)

t.test(dfVan$Mean.Temp,dfCal$Mean.Temp)



t.test(dfKel$Mean.Temp,mu=10)
