# Importing required libraries
library(dplyr)
library(writexl)

# Loading the datasets
full <- read.csv('Dataset/Dataset1.csv')
half1 <- read.csv('Dataset/Dataset2.csv')
half2 <- read.csv('Dataset/Dataset3.csv')

# Grouping January to June data based on Stated.UTs
df_half1 <- group_by(half1, States.UTs)

# Replacing na values by mean
df_Jan <- summarise(df_half1, JAN = mean(JAN, na.rm = T))
for(i in df_Jan$States.UTs)
{
  a <- is.na(half1$JAN)
  b <- half1$States.UTs==i
  half1$JAN[a&b] <- round(df_Jan$JAN[df_Jan$States.UTs==i],2)
}

df_Feb <- summarise(df_half1, FEB = mean(FEB, na.rm = T))
for(i in df_Feb$States.UTs)
{
  a <- is.na(half1$FEB)
  b <- half1$States.UTs==i
  half1$FEB[a&b] <- round(df_Feb$FEB[df_Feb$States.UTs==i],2)
}

df_Mar <- summarise(df_half1, MAR = mean(MAR, na.rm = T))
for(i in df_Mar$States.UTs)
{
  a <- is.na(half1$MAR)
  b <- half1$States.UTs==i
  half1$MAR[a&b] <- round(df_Mar$MAR[df_Mar$States.UTs==i],2)
}

df_Apr <- summarise(df_half1, APR = mean(APR, na.rm = T))
for(i in df_Mar$States.UTs)
{
  a <- is.na(half1$APR)
  b <- half1$States.UTs==i
  half1$APR[a&b] <- round(df_Apr$APR[df_Apr$States.UTs==i],2)
}

df_May <- summarise(df_half1, MAY = mean(MAY, na.rm = T))
for(i in df_May$States.UTs)
{
  a <- is.na(half1$MAY)
  b <- half1$States.UTs==i
  half1$MAY[a&b] <- round(df_May$MAY[df_May$States.UTs==i],2)
}

df_Jun <- summarise(df_half1, JUN = mean(JUN, na.rm = T))
for(i in df_Jun$States.UTs)
{
  a <- is.na(half1$JUN)
  b <- half1$States.UTs==i
  half1$JUN[a&b] <- round(df_Jun$JUN[df_Jun$States.UTs==i],2)
}

# Grouping July to December data based on Stated.UTs
df_half2 <- group_by(half2, States.UTs)

# Replacing na values by mean
df_Jul <- summarise(df_half2, JUL = mean(JUL, na.rm = T))
for(i in df_Jul$States.UTs)
{
  a <- is.na(half2$JUL)
  b <- half2$States.UTs==i
  half2$JUL[a&b] <- round(df_Jul$JUL[df_Jul$States.UTs==i],2)
}

df_Aug <- summarise(df_half2, AUG = mean(AUG, na.rm = T))
for(i in df_Aug$States.UTs)
{
  a <- is.na(half2$AUG)
  b <- half2$States.UTs==i
  half2$AUG[a&b] <- round(df_Aug$AUG[df_Aug$States.UTs==i],2)
}

df_Sep <- summarise(df_half2, SEP = mean(SEP, na.rm = T))
for(i in df_Sep$States.UTs)
{
  a <- is.na(half2$SEP)
  b <- half2$States.UTs==i
  half2$SEP[a&b] <- round(df_Sep$SEP[df_Sep$States.UTs==i],2)
}

df_Oct <- summarise(df_half2, OCT = mean(OCT, na.rm = T))
for(i in df_Oct$States.UTs)
{
  a <- is.na(half2$OCT)
  b <- half2$States.UTs==i
  half2$OCT[a&b] <- round(df_Oct$OCT[df_Oct$States.UTs==i],2)
}

df_Nov <- summarise(df_half2, NOV = mean(NOV, na.rm = T))
for(i in df_Nov$States.UTs)
{
  a <- is.na(half2$NOV)
  b <- half2$States.UTs==i
  half2$NOV[a&b] <- round(df_Nov$NOV[df_Nov$States.UTs==i],2)
}

df_Dec <- summarise(df_half2, DEC = mean(DEC, na.rm = T))
for(i in df_Dec$States.UTs)
{
  a <- is.na(half2$DEC)
  b <- half2$States.UTs==i
  half2$DEC[a&b] <- round(df_Dec$DEC[df_Dec$States.UTs==i],2)
}

# Merging first six months with last six months
df1 <- merge(x=half1, y=half2, by = c("States.UTs", "YEAR"),all.x=TRUE)

# Removing na values in the full(yearly) dataset 
# by replacing it with sum of months of df1
full$ANNUAL <- apply(df1[,3:14], 1, sum)

# Generating new columns (Quarters of the year)
df1$FirstQuarter <- apply(df1[,3:5], 1, sum)

df1$SecondQuarter <- apply(df1[,6:8], 1, sum)

df1$ThirdQuarter <- apply(df1[,9:11], 1, sum)

df1$FourthQuarter <- apply(df1[,12:14], 1, sum)

# Merging all the datasets
df <- merge(x=df1, y=full, by = c("States.UTs", "YEAR"),all.x=TRUE)

# Writing the final dataset to an excel file
write_xlsx(df,"FinalData.xlsx")
