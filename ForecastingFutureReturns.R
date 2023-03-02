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

## Stock Market Data
# VTI  Total Market  - SP500
# VWO	 Total Market  - EMERGING
# QQQ	 Large Cap  - NASDAQ
# XLF  Financial
# DVY	 US - High Dividend Yield  
# LQD  U.S. - Corporate Investment Grade
# AAG  Barclays U.S. Aggregate Index 
# TIP	 U.S. Government TIPS   
# HYG	 U.S. - Corporate High Yield 
# EMB	 Emerging Markets - Sovereign  
# VXX  Volatility short term ETF


#### Long term Data  ####
Des = as.data.table(read.csv("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/AssetAllocation_Global_LT.csv"))
setkey(Des,Index)

LT_BBG=readRDS("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/LT_BBG_Source.rds")
LT_FRED=readRDS("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/LT_FRED_Source.rds")
LT_Yahoo=readRDS("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/ST_Yhaoo_Source.rds")
LT_Carry=readRDS("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/LongTerm_USD_Carry.rds")

setnames(LT_Carry,"Ret_1obs","MontlyRet")
setnames(LT_BBG,"variable","Index")
setnames(LT_FRED,"variable","Index")

setkey(LT_BBG,Index)
setkey(LT_FRED,Index)

LT_BBG=Des[LT_BBG]
LT_FRED=Des[LT_FRED]

setkey(Des,variable)
setkey(LT_Yahoo,variable)

LT_Yahoo=Des[LT_Yahoo][!is.na(Ret_1obs)]

LT_DB=rbind(LT_FRED,LT_BBG,LT_Yahoo)
LT_DB[,CumRet:=100*cumprod(1+ifelse(is.na(Ret_1obs),0,Ret_1obs)),by="variable"]

ggplot(LT_DB[Portfolio=="Dollar"])+
  geom_line(aes(index,CumRet,colour=variable))+
  facet_wrap(.~ Portfolio,scales = "free")


LT_DB_Ret= LT_DB[,list(MontlyRet=sum(Ret_1obs*Direction*Portfolio.Weight)),by=c("index","Portfolio","Type")]

#checking dollar perormance
LT_DB_TRet=rbind(LT_DB_Ret,LT_Carry)
# LT_DB_TRet[,CumRet:=100*cumprod(1+ifelse(is.na(MontlyRet),0,MontlyRet)),by=c("Portfolio","Type")]
# 
# ggplot(LT_DB_TRet[Portfolio=="Dollar"],aes(index,CumRet-100))+
#   geom_area(aes(fill=Type),stat = "identity")+
#   stat_summary(fun = sum, geom = "line", size = 1)

LT_DB_TRet=LT_DB_TRet[,list(MontlyRet=sum(MontlyRet)),by=c("index","Portfolio")]


#till here long term total returns of different asset classes

LT_DB_TRet=LT_DB_TRet[,LongTerm:=100*cumprod(1+ifelse(is.na(MontlyRet),0,MontlyRet)),by="Portfolio"]



#### Volatility Data ####
Vol = LT_DB_TRet[,list(St.Dv=100*sqrt(12)*sd(MontlyRet,na.rm = T)),by="Portfolio"]
Vol$BearMarket = Vol$St.Dv*-2
Vol$MildBearMarket = Vol$St.Dv*-1
Vol$BullMarket = Vol$St.Dv*2
Vol$MildBullMarket = Vol$St.Dv*1
setkey(Vol,Portfolio)

#### Setup for estimate returns based on historical behavior ####
db.work=LT_DB_TRet

var=unique(db.work$Portfolio)

horiz_fwd = 365
horiz_bck = 365



for(i in 1:length(var)){
  
  
  db= xts(db.work[Portfolio == var[i]]$MontlyRet, order.by =db.work[Portfolio == var[i]]$index )
  
  
  for (j in 1:length(index(db))){
  
    date_v = index(db)
    adj=ifelse(j==1,0,1)
    date_s_fwd=index(db)[j+adj] 
    date=index(db)[j]
    
    date_fwd = date+horiz_fwd
    date_back = date-horiz_bck
    
    date_fwd_factor = which(abs(date_v-date_fwd) == min(abs(date_v - date_fwd)))
    date_bck_factor = which(abs(date_v-date_back) == min(abs(date_v - date_back)))
    date_fwd_select = index(db)[date_fwd_factor]
    #added adjustment to factor the way returns are computed
    date_bck_select= index(db)[date_bck_factor]
    date_bck_select_input = index(db)[date_bck_factor+1]
    
    fwd_ret=100*prod(1+db[paste(date_s_fwd, '::', date_fwd_select, sep = ""),],na.rm = T)-100
    back_ret=100*prod(1+db[paste(date_bck_select_input, '::', date, sep = ""),],na.rm = T)-100
    
    
    dataset=data.frame(date=date,Portfolio=var[i],Past=ifelse(date_back<min(date_v),NA,back_ret),
                       Future=ifelse(date_fwd<max(date_v),fwd_ret,NA),Horizon_Future=horiz_fwd,
                       Horizon_Past=horiz_bck,Date_Future = date_fwd_select, Date_Past = date_bck_select)
    
    print(j)
    
    
    if(j==1){
      
     db_selection = dataset
    }else{
      
      db_selection= rbind(db_selection,dataset)
    }
    
    
    
  }
  
  
  if(i==1){
    
    db_final = db_selection
  }else{
    
    db_final = rbind(db_final,db_selection)
    
  }
  
  
  
  
}

#### Credting the stitiscal distribution on observed data ####

db_final_=as.data.table(db_final)
setkey(Vol,Portfolio)
setkey(db_final_,Portfolio)

db_final_= Vol[db_final_]



db_final_[, PastPerfRegime := ifelse(Past< BearMarket,"BearMarket",
                                       ifelse(Past>BullMarket,"BullMarket",
                                              ifelse(Past>BearMarket &  Past< MildBearMarket,"MildBearMarket",
                                                     ifelse(Past> MildBullMarket &Past< BullMarket,"MildBull","Mixed")))),
            by=c("Portfolio")]



db_final_[, Current_State := last(PastPerfRegime),
          by=c("Portfolio")]
db_final_[, Current_Perf := last(Past),
          by=c("Portfolio")]


#### Summary for Expected Returns ####
ExpectedRet_Summary=db_final_[!is.na(PastPerfRegime),list(Median_Ret_Ahead=median(Future,na.rm = T),
                                      Top_Ahead=quantile(Future,0.80,na.rm=T),
                                      Bottom_Ahead=quantile(Future,0.20,na.rm=T),
                                      count = .N,Current_State=last(Current_State),
                                      Current_Perf = last(Current_Perf)),
                                by=c("Portfolio","PastPerfRegime","Horizon_Future","Horizon_Past")]




