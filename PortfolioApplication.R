libs=c("crayon","withr","MASS","zoo","xts","Rblpapi","timeDate",
       "data.table","tidyr","labeling","reshape","digest",
       "ggplot2","reshape2","extrafont","ggrepel","ggthemes",
       "fontcm","RODBC","grid","gridExtra","quantmod")

#install.packages("fontcm",lib="C:/Users/eusep/Documents/Rpacks")

lapply(libs, require, 
       lib.loc = c("C:/Users/eusep/Documents/Rpacks","C:/Program Files/R/R-4.2.0/library"),
       character.only = TRUE)

windowsFonts(
  Candara = windowsFont("Bahnschrift"), #the font we need to apply
  B = windowsFont("Bookman Old Style"),
  C = windowsFont("Comic Sans MS"),
  D = windowsFont("Symbol")
)
loadfonts()

rm(list = ls())

source("C:/Users/eusep/OneDrive/Documenti/R/ChartLayout.R")
VarDes = as.data.table(read.csv("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/AssetAllocation_Global_LT.csv"))
Data_BBG=readRDS("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/LT_BBG_Source.rds")
setnames(Data_BBG,"variable","Index")
setkey(Data_BBG,Index)
setkey(VarDes,Index)
Data_BBG = VarDes[Data_BBG]

Data_FRED=as.data.table(readRDS("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/LT_FRED_Source.rds"))
setnames(Data_FRED,"variable","Index")
setkey(Data_FRED,Index)
Data_FRED = VarDes[Data_FRED]
Data_FRED = Data_FRED[, Ret_1obs:= value/c(NA,value[-.N])-1, by="variable"]

Data_LT = rbind(Data_BBG,Data_FRED)

Data_Yah=readRDS("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/ST_Yhaoo_Source.rds")
Data_Yah = unique(Data_Yah)
setkey(VarDes,variable)
setkey(Data_Yah,variable)

Data_Yah = VarDes[Data_Yah]

#Full Long Term monthly data DB - USD returns need to complemented by carry
Full_LT =  rbind(Data_LT,Data_Yah)

Full_LT_Port=Full_LT[,list(Ret_1obs = sum(Portfolio.Weight*Direction*Ret_1obs)),by=c("Portfolio","Type","index")]
DollarCarry=readRDS("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/LongTerm_USD_Carry.rds")
DollarCarry$Ret_1obs=DollarCarry$Ret_1obs/12

Full_LT_Port.1=rbind(DollarCarry,Full_LT_Port)

#Full Long Term monthly data DB - USD returns with carry
Full_LT_Port.2=Full_LT_Port.1[,list(Ret_1obs=sum(Ret_1obs)),by=c("index","Portfolio")]


