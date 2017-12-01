setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
monthList <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
locList <- c("Vancouver", "Kelowna", "Calgary")
DataAll <- read.csv("all_CLEANED.csv", header = TRUE)
DataAll$Date.Time <- as.Date(DataAll$Date.Time)
DataAll$Month <- factor(DataAll$Month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = monthList);
DataAll$City <- factor(DataAll$City, levels = c(1,2,3), labels = locList);
library(forecast)


df <- subset(DataAll, City == "Vancouver" & Year >= 2000 & Year <= 2017)
monthly.df <- ddply(df,.(Year, Month), summarize, meanT= mean(Mean.Temp, na.rm = TRUE))
monthlyFit <- ts(monthly.df$meanT)
fit <- auto.arima(monthlyFit)


  df <- subset(DataAll, City == "Kelowna" & Year >= 2000 & Year <= 2017)
  monthly.df <- ddply(df,.(Year, Month), summarize, meanT= mean(Mean.Temp, na.rm = TRUE))
  monthlyFit <- ts(monthly.df$meanT)
  fit2 <- auto.arima(monthlyFit)


  df <- subset(DataAll, City == "Calgary" & Year >= 2000 & Year <= 2017)
  monthly.df <- ddply(df,.(Year, Month), summarize, meanT= mean(Mean.Temp, na.rm = TRUE))
  monthlyFit <- ts(monthly.df$meanT)
  fit3 <- auto.arima(monthlyFit)

names <- c("Mean", "City")
df1 <- data.frame(matrix(ncol = 2, nrow = 216)) 
colnames(df1) <- names
df2 <- data.frame(matrix(ncol = 2, nrow = 204)) 
colnames(df2) <- names
df3 <- data.frame(matrix(ncol = 2, nrow = 216)) 
colnames(df3) <- names

sim <- simulate(fit,future=FALSE)
df1$Mean <- as.numeric(sim)
df1$City <- "Vancouver"
sim2 <- simulate(fit2,future=FALSE)
df2$City <- "Kelowna"
df2$Mean <- as.numeric(sim2)
sim3 <- simulate(fit3,future=FALSE)
df3$City <- "Calgary"
df3$Mean <- as.numeric(sim3)

df <- rbind(df1, df2, df3)
summary(aov(df$Mean~df$City))

