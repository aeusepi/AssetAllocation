libs=c("crayon","withr","MASS","zoo","xts","Rblpapi","timeDate",
       "data.table","tidyr","labeling","reshape","digest","FRAPO",
       "ggplot2","reshape2","extrafont","ggrepel","ggthemes",
       "fontcm","RODBC","grid","gridExtra","quantmod","Quandl")

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
EUR_1973_1974=as.data.table(read.csv("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/EURO_Equivalent_Ret_1973_1974.csv"))

conn <- blpConnect()

#### Download data from BBG all the data set are ready only the yahoo data needs to be refreshed ####

BBGList=VarDes[Source=="BBG"& Source2 != "QUANDL"]$Index
opt <- c("periodicitySelection" = "MONTHLY")
Data.Assets = bdh(BBGList,"PX_LAST",start.date = as.Date("1972-12-01"),
               end.date =  as.Date("2018-01-01"),options = opt,include.non.trading.days = FALSE,
               returnAs = "xts")

if("DataBaseRates" %in% ls()){rm("DataBaseRates")}
for (i in 1:length(BBGList)){


  temp = as.data.table(Data.Assets[[i]])
  setkey(temp,index)
  setnames(temp,"PX_LAST",names(Data.Assets[i]))



  if(i == 1){

    DataBaseRates = temp

  }else{


    setkey(DataBaseRates,index)
    DataBaseRates =temp[DataBaseRates]

  }



}


missing=read.csv("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/Missing_LT_obs.csv")
missing$index=as.Date(missing$index,format = "%d/%m/%Y")

data=as.data.table(melt(DataBaseRates,id="index"))
data = as.data.table(rbind(missing,data))

data[, Ret_1obs := value/c(NA,value[-.N])-1,by="variable"]
data[variable == "USDEUR Curncy" & is.na(Ret_1obs) & index != "1972-12-29"]$Ret_1obs = EUR_1973_1974$Returns
saveRDS(data, "C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/LT_BBG_Source.rds")


# Download data from FRED 
LT_Currency=new.env()
FREDList = VarDes[Source=="FRED"]$Index
getSymbols(FREDList,
           src='FRED',env = LT_Currency, return.class="xts")


pframe_LTC <- as.data.table(do.call(merge, as.list(LT_Currency)))
pframe_LTC.melt=melt.data.table(pframe_LTC,id.vars = "index")[index> "1972-12-01" & index < "2018-01-01"]
pframe_LTC.melt[,value := na.locf(value,na.rm = F,fromLast=FALSE),
                by="variable"]
pframe_LTC.melt$MMY= format(pframe_LTC.melt$index,"%Y-%b")
data_fred = pframe_LTC.melt[,list(index=max(index),value=last(value)),by=c("variable","MMY")]
data_fred$MMY=NULL
data_fred[,Ret_1obs := value/c(NA,value[-.N])-1, by="variable"]
data_fred[,index := ifelse(weekdays(index,T)=="Sun",index-2,ifelse(weekdays(index,T)=="Sat",index-1,index)), by="variable"]



saveRDS(data_fred, "C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/LT_FRED_Source.rds")

#### download data from yahoo ####

SMdata= VarDes[Source2 == "Yahoo"]$Ticker


substart = as.Date("2017-12-29") # date selected when the ICOM was launched
subend   = Sys.Date()-1

myenv = new.env()

symnames = getSymbols(SMdata, src="yahoo", env=myenv,from = substart,
                      to = subend)  

#the "Ad" function is just an abbreviation for Adjusted.CLose
AdPrice = do.call(merge, eapply(myenv, Cl)[symnames])
AdPrice = AdPrice[complete.cases(AdPrice)]
AdPrice.dt=melt.data.table(as.data.table(AdPrice),id ="index")
setkey(AdPrice.dt,index,variable)

# retrieving data for dividends to compute the total return
mayenv=new.env()

for (i in 1:length(SMdata)){
  getDividends(SMdata[i],
               from = substart,
               to = subend, 
               env = mayenv, 
               src = "yahoo", 
               auto.assign = TRUE, 
               auto.update = TRUE, 
               verbose = FALSE)}

div=do.call(merge,(eapply(mayenv,merge)))
div.n=sub(".div","",names(div))
check = SMdata %in% div.n

if (FALSE %in% check){
  
  div$ICON.L.div = NA

}


nn=sort(names(div))
div = div[,nn]

#creating TTR vector


SMdata.div= matrix(NA,ncol = length(SMdata),nrow=1)
for (i in 1:length(SMdata)){
  SMdata.div[i]=paste(SMdata[i],".div",sep="")
}

SMdata.div=div
remove(div)
SMdata=sort(SMdata)

#the two dataset are ordered
SMdata.div=SMdata.div[,order(colnames(SMdata.div))]
AdPrice=AdPrice[,order(colnames(AdPrice))]
ClPrice=AdPrice
rm(AdPrice)



# create sample data

newenv=new.env()
for (i in 1:length(SMdata)){
  
  SPY.Close = ClPrice[,i]
  SPY.Div = SMdata.div[,i]
  SPY = merge(SPY.Close, SPY.Div)
  # now adjust close for dividends
  ratios = adjRatios(dividends=SPY[,2], close=SPY[,1])
  SPY$SPY.Adjusted <- (ratios$Split * ratios$Div) * SPY[,1]
  # only keep dates from the original object
  SPY <- SPY[index(SPY.Close),]
  names(SPY) = sub(paste("SPY",".Adjusted",sep=""),
                   paste(SMdata[i],".TTR",sep=""), names(SPY))
  assign(SMdata[i],SPY[,3],env=newenv)
  
  
  rm(SPY.Close)
  rm(SPY)
  rm(SPY.Div)
  rm(ratios)
}




TTRdata=do.call(merge,eapply(newenv,merge))
TTRdata=TTRdata[complete.cases(TTRdata),order(colnames(TTRdata))]
ClPrice =ClPrice[complete.cases(ClPrice),order(colnames(ClPrice))]

for (i in 1:length(SMdata) ){
  names(ClPrice) = sub(paste(SMdata[i],".Close",sep=""),
                       paste(SMdata[i],".EXD",sep=""), names(ClPrice))
}


TTRdata1 = as.ts(TTRdata)
ClPrice1 = as.ts(ClPrice)

RetTR=returnseries(TTRdata1, method = c("discrete"), percentage = FALSE,
                   trim = FALSE, compound = FALSE)

Ret=returnseries(ClPrice1, method = c("discrete"), percentage = FALSE,
                 trim = FALSE, compound = FALSE)




# Total return
full.dt1=melt.data.table(as.data.table(TTRdata),id="index")
full.dt1$variable=sub(".TTR","",full.dt1$variable)
full.dt1$Type="Total Return"

#ex dividend
full.dt=melt.data.table(as.data.table(ClPrice),id="index")
full.dt$variable=sub(".EXD","",full.dt$variable)
full.dt$Type="Ex. Dvd"

f.dt = rbind(full.dt,full.dt1)

f.dt[, Ret_1D := value/c(NA,value[-.N])-1,by=c("variable","Type")]
f.dt[, CumRet := 100*cumprod(1+ifelse(is.na(Ret_1D),0,Ret_1D)),by=c("variable","Type")]
f.dt[, MMYY:= format(index, "%b-%Y")]

#quick visualization of the time series
ggplot(data=f.dt)+
  geom_line(aes(x=index,y=CumRet,color=Type))+
  facet_wrap(.~variable, ncol = 1,  scales = "free")

data_yahoo_monthly=f.dt[Type=="Total Return",list(index=last(index),value=last(value)),by=c("variable","MMYY")]
data_yahoo_monthly[,Ret_1obs := value/c(NA,value[-.N])-1,by="variable"]
data_yahoo_monthly$MMYY=NULL

# for currency 

Dollar= VarDes[Source2 == "Yhaoo2"]$Ticker


substart = as.Date("2017-12-29") # date selected when the ICOM was launched
subend   = Sys.Date()-1

myenv = new.env()

symnames = getSymbols(Dollar, src="yahoo", env=myenv,from = substart,
                      to = subend)  

#the "Cl" function is just an abbreviation for Adjusted.CLose
AdPrice = do.call(merge, eapply(myenv, Cl)[symnames])
AdPrice = AdPrice[complete.cases(AdPrice)]
names(AdPrice)= sub(".X.Close","",names(AdPrice))
data_daily= as.data.table(melt(as.data.table(AdPrice),id="index"))
data_monthly =data_daily
data_monthly[, MMYY:= format(index, "%b-%Y")]

data_monthly=data_monthly[,list(index=last(index),value=last(value)),by=c("variable","MMYY")]

data_monthly[,value:= ifelse((variable=="USDJPY"),value,1/value),by=c("variable","index","MMYY")]
data_monthly[,variable:= ifelse(variable=="GBPUSD","USDGBP",ifelse(variable=="EURUSD","USDEUR",variable)),by=c("variable","index","MMYY")]

data_monthly[,Ret_1obs := value/c(NA,value[-.N])-1,by="variable"]
data_monthly$MMYY=NULL

# Merging the two data set
data_yahoo_monthly=rbind(data_yahoo_monthly,data_monthly)
saveRDS(data_yahoo_monthly, "C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/ST_Yhaoo_Source.rds")


# Creating Short Term Daily Data Set

DailyETF = f.dt[Type=="Total Return"][,list(index,variable,Ret_1obs=Ret_1D)]
DailyFX = data_daily[,list(index,Ret_1obs = value/c(NA,value[-.N])-1), by="variable"]

Daily_Data = rbind(DailyETF,DailyFX)


saveRDS(Daily_Data, "C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/ST_Yhaoo_Source_Daily.rds")

# Carry trade data USD vs EUR, GBP, JPY - check the column data from 

Dollar= VarDes[Source2 == "QUANDL"]$Ticker
Refccy= VarDes[Source2 == "QUANDL"]$variable

refSet= list(tick=c("Date",Dollar),names=c("index",Refccy))

mydata = Quandl(Dollar)
colatt=sub(" - Percent per annum","",names(mydata))
colatt=sub("\\.","/",colatt)
coordinate = match(colatt,refSet$tick)
names(mydata) = refSet$names[coordinate]

ccyData= melt.data.table(as.data.table(mydata),id="index")[!is.na(value)]
ggplot(ccyData)+
  geom_line(aes(index,value,color=variable))

historicalData=read.csv("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/BIS_PolicyRate_DiscountRateBundesbank .csv")
historicalData$index=seq(as.Date("1973-02-01"),length=252,by="months")-1

historicalData$Period = NULL

historicalData=melt.data.table(as.data.table(historicalData),id="index")[!is.na(value)]

CarryData = rbind(historicalData,ccyData)
CarryData[,index:= ifelse(weekdays(index,TRUE)=="Sun",index-2,
                          ifelse(weekdays(index,TRUE)=="Sat",index-1,index))]

CarryData$index=as.Date(CarryData$index)

ggplot(CarryData)+
  geom_line(aes(index,value,color=variable))

setkey(VarDes,variable)
setkey(CarryData,variable)

Data=VarDes[CarryData]
USDCarry = Data[,list(Ret_1obs =  sum(Portfolio.Weight*Direction*value)/100/12), 
                by=c("Portfolio","Type","index")]

ggplot(USDCarry)+
  geom_line(aes(index,Ret_1obs,color=Portfolio))


saveRDS(USDCarry, "C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/LongTerm_USD_Carry.rds")

USDCarry[,CumRet:= 100*cumprod(1+Ret_1obs), by=c("Portfolio","Type")]
ggplot(USDCarry)+
  geom_line(aes(index,CumRet,color=Portfolio))





