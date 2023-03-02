libs = c("FRAPO","tidyverse","rvest","XML","RCurl","rlist","stringi","ggplot2", "ggthemes",  "ggrepel","FactoMineR","corrplot",
         "data.table","quantmod","priceR","reshape","PerformanceAnalytics","factoextra","extrafont")
#install.packages("cccp",lib="C:/Users/eusep/Documents/Rpacks")

.libPaths(c("C:/Users/eusep/Documents/Rpacks","C:/Program Files/R/R-4.2.0/library"))

lapply(libs, require, 
       lib.loc = c("C:/Users/eusep/Documents/Rpacks","C:/Program Files/R/R-4.2.0/library"),
       character.only = TRUE)

rm(list=ls())
source("C:/Users/eusep/OneDrive/Documenti/R/ChartLayout.R")
loadfonts(device = "win")

Des = as.data.table(read.csv("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/AssetAllocation_Global_LT_Weekly.csv"))
setkey(Des,Index)

WeeklyData=readRDS("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/LongTerm_Weekly1989_2018.rds")
DailyData=readRDS("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/ShortTermWeekl_280717.rds")
DailyData$WWMMYY= weekdays(DailyData$index,abbreviate = T)
DailyData=DailyData[WWMMYY == "Fri" &   index > "2017-12-29"]
setkey(WeeklyData,Index)

Db_weekly = Des[WeeklyData]
Db_weekly[,Ret_1D:= value/c(NA,value[-.N])-1, by="variable"]

LagacyWeekly = Db_weekly[,list(variable,Date,Ret_1D)]
NewWeekly=DailyData[,list( Date=index, variable,Ret_1D)]

FullWeekly = rbind(LagacyWeekly,NewWeekly)
FullWeekly.Analysis = as.matrix(cast(FullWeekly,Date~variable,fun.aggregate = sum, value = "Ret_1D"))[-1,]


#### Analysis Start ####
chart.Correlation(FullWeekly.Analysis, histogram=TRUE, pch="+")


