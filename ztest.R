setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
monthList <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
locList <- c("Vancouver", "Kelowna", "Calgary")
DataAll <- read.csv("all_CLEANED.csv", header = TRUE)
DataAll$Date.Time <- as.Date(DataAll$Date.Time)
DataAll$Month <- factor(DataAll$Month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = monthList);
DataAll$City <- factor(DataAll$City, levels = c(1,2,3), labels = locList);
library(forecast)

df <- subset(DataAll, City == "Calgary" & Year >= 2000 & Year <= 2017)
monthly.df <- ddply(df,.(Year, Month), summarize, meanT= mean(Mean.Temp, na.rm = TRUE))
monthlyFit <- ts(monthly.df$meanT)
fit <- auto.arima(monthlyFit)
sim <- simulate(fit,future=FALSE)




