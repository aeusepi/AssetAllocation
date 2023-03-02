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

SMdata=c("VTI","AGG","VWO","EMB","HYG")

substart = as.Date("2012-09-05")
subend   = as.Date("2022-09-02")

myenv = new.env()

symnames = getSymbols(SMdata, src="yahoo", env=myenv,from = substart,
                      to = subend )  

#the "Ad" function is just an abbreviation for Adjusted.CLose
AdPrice = do.call(merge, eapply(myenv, Cl)[symnames])
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
head(div)
names(div)=sort(names(AdPrice))

#### Selection of indices to Run a long term asset allocation ####


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
for (i in 1:length(SMdata) ){
  names(ClPrice) = sub(paste(SMdata[i],".Adjusted",sep=""),
                       paste(SMdata[i],".Close",sep=""), names(ClPrice))
}



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
TTRdata=TTRdata[,order(colnames(TTRdata))]

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


# ex.dvd return data base 
full.dt.ed=melt.data.table(as.data.table(ClPrice1),id="index")


# total return data

full.dt1=melt.data.table(as.data.table(TTRdata),id="index")
full.dt1$variable=sub(".TTR","",full.dt1$variable)
full.dt1$Type="Total Return"

full.dt=melt.data.table(as.data.table(ClPrice),id="index")
full.dt$variable=sub(".EXD","",full.dt$variable)
full.dt$Type="Ex. Dvd"

f.dt = rbind(full.dt,full.dt1)

f.dt[, Ret_1D := value/c(NA,value[-.N])-1,by=c("variable","Type")]
f.dt[, CumRet := 100*cumprod(1+ifelse(is.na(Ret_1D),0,Ret_1D)),by=c("variable","Type")]
f.dt[, MMYY:= format(index, "%b-%Y")]

ggplot(data=f.dt)+
  geom_line(aes(x=index,y=CumRet,color=Type))+
  facet_wrap(.~variable, ncol = 1,  scales = "free")


setting="Daily"
#set up horizon for factor 
if (setting=="Daily"){
  sixMonth=126
  oneYear=252
}else if(setting=="Weekly"){
  sixMonth=26
  oneYear=52
}

ExpectedRet = f.dt[Type=="Total Return" & !is.na(Ret_1D),
                   list(index,Ret_1D,
                        Ret_past=100*rollapply(data= (1+Ret_1D),width = oneYear, by=1,
                                                   FUN=prod,fill=NA, align ="right",na.rm=F)-100,
                        Ret_6M_ahead=100*rollapply(data= (1+Ret_1D),width = sixMonth, by=1,
                                                   FUN=prod,fill=NA, align ="left",na.rm=F)-100),
                   by=c("variable","Type")]


ExpectedRet[, PastPerfRegime := ifelse(Ret_6M_past< (-20),"BearMarket",
                                       ifelse(Ret_6M_past>20,"BullMarket",
                                              ifelse(Ret_6M_past> -20 &  Ret_6M_past< -5,"MildBearMarket",
                                                     ifelse(Ret_6M_past> 5 &  Ret_6M_past< 20,"MildBull","Mixed")))),
by=c("variable","Type")]

ExpectedRet[, MMYY:= format(index, "%b-%Y")]



ExpectedRet_Summary=ExpectedRet[,list(Median_Ret_6M_Ahead=median(Ret_6M_ahead,na.rm = T),
                                      Top75_6M_Ahead=quantile(Ret_6M_ahead,0.75,na.rm=T),
                                      Bottom25_6M_Ahead=quantile(Ret_6M_ahead,0.25,na.rm=T)),
                                by=c("variable","Type","PastPerfRegime")]



#Statistics

# SkewRet=VolatilitySkewness(Ret, MAR=0,stat = c("volatility"))
# SkewRetTR=VolatilitySkewness(RetTR, MAR=0,stat = c("volatility"))
# CumRet=Return.cumulative(Ret,geometric=FALSE)*100/as.numeric((subend-substart)/365)
# CumRetTR=Return.cumulative(RetTR,geometric=FALSE)*100/as.numeric((subend-substart)/365)
# Vol1yRet=StdDev.annualized(Ret,scale=252)*100
# Vol1yRetTR=StdDev.annualized(RetTR,scale=252)*100
# 
# Refport=xts(drop(merge.xts(Ret$TIP.EXdiv[-1,],Ret$VTI.EXdiv[-1,]))%*%
#               as.matrix(c(0.5,0.5),nrow=2), order.by = index(Ret$VTI.EXdiv[-1,]))
# RefportTR=xts(drop(merge.xts(RetTR$TIP.TTR[-1,],RetTR$VTI.TTR[-1,]))%*%
#               as.matrix(c(0.5,0.5),nrow=2), order.by = index(RetTR$VTI.TTR[-1,]))
# 
# CorrRet=cor(merge(Ret[-1,],Refport),use = "pairwise.complete.obs")
# CorrRet=CorrRet[,ncol(CorrRet)]
# CorrRetTR=cor(merge(RetTR[-1,],RefportTR),use = "pairwise.complete.obs")
# CorrRetTR=CorrRetTR[,ncol(CorrRetTR)]
# 
# ggplot(Vol1yRetTR,CumRetTR)


RetTR=coredata(RetTR)
RetTR = RetTR[-1,]
Ret= coredata(Ret)
Ret = Ret[-1,]




#### Efficient Frontier function ####
eff.frontier <- function (returns, short="no", max.allocation=NULL,
                          risk.premium.up=.5, risk.increment=.005,
                          frequency=252){
  # return argument should be a m x n matrix with one column per security
  # short argument is whether short-selling is allowed; default is no (short
  # selling prohibited)max.allocation is the maximum % allowed for any one
  # security (reduces concentration) risk.premium.up is the upper limit of the
  # risk premium modeled (see for loop below) and risk.increment is the
  # increment (by) value used in the for loop
  # frequency is the return computantion convention in order to annualize the 
  # final results (yearly,weekly,monthly,daily)
  
  covariance <- cov(returns)
  #print(covariance)
  n <- ncol(covariance)
  
  # Create initial Amat and bvec assuming only equality constraint
  # (short-selling is allowed, no allocation constraints)
  Amat <- matrix (1, nrow=n)
  bvec <- 1
  meq <- 1
  
  # Then modify the Amat and bvec if short-selling is prohibited
  if(short=="no"){
    Amat <- cbind(1, diag(n))
    bvec <- c(bvec, rep(0, n))
  }
  
  # And modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(max.allocation)){
    if(max.allocation > 1 | max.allocation <0){
      stop("max.allocation must be greater than 0 and less than 1")
    }
    if(max.allocation * n < 1){
      stop("Need to set max.allocation higher; not enough assets to add to 1")
    }
    Amat <- cbind(Amat, -diag(n))
    bvec <- c(bvec, rep(-max.allocation, n))
  }
  
  # Calculate the number of loops
  loops <- risk.premium.up / risk.increment + 1
  loop <- 1
  
  # Initialize a matrix to contain allocation and statistics
  # This is not necessary, but speeds up processing and uses less memory
  eff <- matrix(nrow=loops, ncol=n+3)
  # Now I need to give the matrix column names
  colnames(eff) <- c(colnames(returns), "Std.Dev", "Exp.Return", "sharpe")
  
  # Loop through the quadratic program solver
  for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
    dvec <- colMeans(returns) * i # This moves the solution along the EF
    sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
    eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution*colSums((covariance*sol$solution))))
    eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% colMeans(returns))
    eff[loop,"sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
    eff[loop,1:n] <- sol$solution
    loop <- loop+1
  }
  
  eff=as.data.frame(eff)
  
  if (frequency=="yearly"){
    freq=1
  }else if(frequency=="monthly"){
    freq=12
  }else if(frequency=="weekly"){
    freq=52
  }else
    freq=252
  
  eff$Std.Dev=eff$Std.Dev*sqrt(freq)*100
  eff$Exp.Return=eff$Exp.Return*freq*100
  eff$sharpe=eff$sharpe*sqrt(freq)
  
  return(eff)
}

#### Run the eff.frontier function based on no short and 50% alloc. restrictions


eff = eff.frontier(returns=Ret[-1,], short="yes", max.allocation=.50,
                    risk.premium.up=0.05, risk.increment=.001,
                   frequency="daily")
eff1 = eff.frontier(returns=RetTR[-1,], short="yes", max.allocation=.50,
                    risk.premium.up=0.05, risk.increment=.001,
                    frequency="daily")

# Find the optimal portfolio

eff.optimal.point = eff[eff$sharpe==max(eff$sharpe),]
eff.optimal.point1= eff1[eff1$sharpe==max(eff1$sharpe),]
# graph efficient frontier
# Start with color scheme
ealred <- "#8B0000" # color of the frontier
ealtan <- "#CDC4B6"
eallighttan <- "#FDF5E6"# #background, title name, point name
ealdark <- "#000080" #Axis label
text="#000000"

#geom_point controls the background

ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=1, color=ealdark) +
  geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
             color=ealred, size=4) +
  annotate(geom="text", x=eff.optimal.point$Std.Dev,
           y=eff.optimal.point$Exp.Return,
           label=paste("Annualized Risk: ",
                       round(eff.optimal.point$Std.Dev, digits=2),"%","\nAnnualized Return: ",
                       round(eff.optimal.point$Exp.Return, digits=2),"%\nAnnualized Sharpe: ",
                       round(eff.optimal.point$sharpe, digits=2), sep=""),
           hjust=0, vjust=1.2,size=4) + scale_y_continuous(limits = c(3, 25)) +
  scale_x_continuous(limits = c(3, 10)) +
  ggtitle("Efficient Frontier\nand Optimal Portfolio") +
  labs(x="Risk (standard deviation of portfolio) %", y="Return %") +
  theme(panel.background=element_rect(fill=eallighttan),
        text=element_text(size=12,color=text),
        plot.title=element_text(size=24, color=ealred))
ggsave(paste("Efficient Frontier 2Y-Window up to ",index(last(ClPrice)),".png",sep=""))


ggplot(eff1, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=1, color=ealdark) +
  geom_point(data=eff.optimal.point1, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
             color=ealred, size=4) +
  annotate(geom="text", x=eff.optimal.point1$Std.Dev,
           y=eff.optimal.point1$Exp.Return,
           label=paste("Annualized Risk: ",
                       round(eff.optimal.point1$Std.Dev, digits=3),"%","\nAnnualized Return: ",
                       round(eff.optimal.point1$Exp.Return, digits=4),"%\nAnnualized Sharpe: ",
                       round(eff.optimal.point1$sharpe, digits=2), sep=""),
           hjust=0, vjust=1.2,size=4) +scale_y_continuous(limits = c(3, 25))+
  scale_x_continuous(limits = c(3, 10))+
  ggtitle("Efficient Frontier\nand Optimal Portfolio") +
  labs(x="Risk (standard deviation of portfolio) %", y="Return %") +
  theme(panel.background=element_rect(fill=eallighttan),
        text=element_text(color=text),
        plot.title=element_text(size=24, color=ealred))
ggsave(paste("Efficient Frontier(TR) 2Y-Window up to ",index(last(ClPrice)),".png",sep=""))


########################## Correlation Heatmap ###########################


png("CorrlHeatmap.png", units="px", width=4000, height=4000, res=700)
ggplot(data = melt(cor(RetTR)), aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#8B0000", high = "#00008B", mid = "#F0F8FF", 
                       midpoint = 0, limit = c(-1,1), name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
dev.off()

######################### PCA ############################################
library("psych")
library("Jmisc")
p = princomp(na.omit(RetTR))

loadings = p$loadings[]

# look at the first 4 principal components  
components = loadings[,1:4]

# normalize all selected components to have total weight = 1
components = components / repRow(colSums(abs(components)), length(SMdata))

# note that first component is market, and all components are orthogonal i.e. not correlated to market
n=length(SMdata)
market = RetTR %*% rep(1/n,n) # equally weighted portfolio 

temp = cbind(market, RetTR %*% components)
colnames(temp)[1] = 'Market'   

round(cor(temp, use='complete.obs',method='pearson'),3)

# the variance of each component is decreasing
VAR=t(matrix(round(100*SD(temp,na.rm=T),2)))
colnames(VAR)=t(matrix(colnames(temp)))

#*****************************************************************
# Find stationary components, Augmented Dickey-Fuller test
#******************************************************************     
library(tseries)
setInternet2(TRUE)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)


equity = bt.apply.matrix(1 + ifna(-RetTR %*% components,0), cumprod)
equity = xts(equity,order.by = index(ClPrice[-1,]))

# test for stationarity ( mean-reversion )
adf.test(as.numeric(equity[,1]))$p.value
adf.test(as.numeric(equity[,2]))$p.value
adf.test(as.numeric(equity[,3]))$p.value
adf.test(as.numeric(equity[,4]))$p.value




##########################################################################

### H2O AssetClass
setwd("P:/Services/H2O/Quant/Development/Projects/Ongoing/EfficientFrontierAnalysis")
mydata = read.csv("Force10.csv",header=TRUE)
date=as.Date(as.character(mydata[-1,1],"%Y/%m/%d"))
riskfs=as.xts(mydata[-1,-1],order.by = date)
VARCOV=cov(riskfs)

