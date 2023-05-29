# Set UP ####
libs=c("ggplot2","digest","zoo","xts","Rblpapi","timeDate","crayon","pillar",
       "data.table","tidyr","labeling","reshape","withr","quadprog",
       "reshape2","extrafont","gtable","ggrepel","corpcor","matrixcalc",
       "fontcm","RODBC","grid","gridExtra","farver","corrplot","BLCOP",
       "fPortfolio","PortfolioAnalytics","MASS","ggthemes","nloptr","mvtnorm","Matrix")

## Library local paths ####

info  = Sys.info()
if (info["login"]=="eusea"){
  lib_loc = "C:/Users/eusea/Rpacks"
  printloc = "C:/Users/eusea/R_charts_output"
}else{
  lib_loc = c("C:/Users/eusep/Documents/Rpacks","C:/Program Files/R/R-4.2.0/library")
  printloc = "C:/Users/eusep/OneDrive/Documenti/Macro Framework/Database"
}

.libPaths(c(lib_loc))
lapply(libs, require, 
       lib.loc = lib_loc,
       character.only = TRUE)
# lapply(lib2, require, 
#        lib.loc = lib_loc,
#        character.only = TRUE)
# 
# install.packages("rlang")

loadfonts(device = "win")

source("ChartLayout.R")

windowsFonts(
  A = windowsFont("Bahnschrift"), #the font we need to apply
  B = windowsFont("Bookman Old Style"),
  C = windowsFont("Comic Sans MS"),
  D = windowsFont("Symbol")
)
loadfonts()

rm(list = ls())

source("EfficientFrontCredit.R")
source("ChartLayout.R")


# load("CarryDataCcy.rda")
# load("FX_Rates_Returns_All.rda")

#ReturnDataTotal=ReturnDataTotal1
#1. Creating the ptf expected returns for AUD portfolio  -----

mmkt_assets_des = as.data.table(read.csv("DataDesMMkt.csv"))

mmkt_assets =  as.data.table(read.csv("MoneyMktHistoric.csv"))
mmkt_assets$date= as.Date(mmkt_assets$date,format = '%m/%d/%Y')

mmkt_assets_carry = mmkt_assets[Type=='EY']
mmkt_assets_carry$Type=NULL
mmkt_assets_return = mmkt_assets[Type=='TTR']
mmkt_assets_return$Type=NULL

# ##1.1 homogeneous data series on the carry ====
# asset_test = as.data.table(melt(mmkt_assets_carry,id="date"))
# asset_test= asset_test[,list(date,variable,
#                              value=na.locf(value,na.rm = FALSE))]
# 
# setkey(asset_test,variable)
# setkey(mmkt_assets_des,variable)
# asset_test_ = asset_test[mmkt_assets_des]
# ##1.2 Combination of single instrument in oreder to create asset carry ====
# asset_db_last=asset_test_[date==last(date)]
# 
# # Asset Data Selection ####
# assetSelection = unique(asset_db_last$variable)
# ### 1.2.1 Plotting data #####
# carry=ggplot(data=asset_db_last[variable %in% assetSelection],
#              aes(x=variable,y=value,
#                  fill=variable,label=ifelse(round(value,2))))+
#   geom_bar(stat = 'identity')+
#   geom_text(size = 3, check_overlap = TRUE, position = position_stack(vjust = 0.5),
#             family="Bahnschrift")+
#   theme(panel.background=element_rect(fill="white"),
#         panel.grid.major =element_line(colour = "gray85",linetype = "dashed"),
#         #text=element_text(size=12,color="black"),
#         plot.title=element_text(size=20, color="darkred"))+
#   theme(axis.text.y = element_text(color = "black", size = 10, family = "Bahnschrift"),
#         axis.text.x = element_text(color = "black", size = 14, family = "Bahnschrift"),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text=element_text(size=8,family = "Bahnschrift",color = "White"))+
#   theme(legend.position = "bottom",
#         legend.text = element_text(family = "Bahnschrift", size = 10),
#         legend.title = element_blank())+
#   geom_hline(yintercept=0,linetype="dashed", color="black")+
#   coord_flip()
# 
# ggsave(paste("carryUSD",".png",sep=""), plot = carry, device = png,
#        scale = 1, width = 12, height = 18, units = c("cm"),
#        dpi = 100)

##1.3 Plotting evolution of specif time series ====

# 
# asset_db_evo = asset_db[Portfolio %in% assetSelection]
# asset_db_evo=asset_db_evo[,list(value=sum(value*Weight),rk=mean(Rk)),
#                             by=c("Type","Portfolio","date")]
# 
# #evolution of the carry component and percentile ranking ove rthe last 5yrs
# USD_Carry=ggplot()+
#   geom_area(data=asset_db_evo[],aes(date,ifelse(Type=="FX_Carry",-value,value),fill=Portfolio),
#            stat='identity')+
#   scale_fill_manual(values = c("coral","lightblue","lightgreen"))+
#   facet_wrap(.~Portfolio, ncol = 1)+
#   scale_y_continuous(limits = (c(-15,10)),breaks = seq(-15,10,5))+
#   geom_text(size = 3, check_overlap = TRUE, position = position_stack(vjust = 0.5),family="Bahnschrift")+
#   theme(panel.background=element_rect(fill="white"),
#         panel.grid.major =element_line(colour = "gray85",linetype = "dashed"),
#         #text=element_text(size=12,color="black"),
#         plot.title=element_text(size=20, color="darkred"))+
#   theme(axis.text.y = element_text(color = "black", size = 12, family = "Bahnschrift"),
#         axis.text.x = element_text(color = "black", size = 12, family = "Bahnschrift"),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         strip.text.x = element_text(size = 10, family ="Bahnschrift"),
#         axis.text=element_text(size=10,family = "Bahnschrift",color = "White"))+
#   theme(legend.position = "bottom",
#         legend.text = element_text(family = "Bahnschrift", size = 10),
#         legend.title = element_blank())+
#   geom_hline(yintercept=0,linetype="dashed", color="black")
# 
# ggsave(paste("carryUSD_ts",".png",sep=""), plot = USD_Carry, device = png,
#        scale = 1, width = 13, height = 15, units = c("cm"),
#        dpi = 100)


#1.4 Data elaboration for risk-return measures ####
## 1.4.2 Return analysis #### 
# Creation of asset carry from the combination of instrument carry
# CarryDataTotal = asset_db[,list(value=sum(value*Weight),rk=mean(Rk)),
#                           by=c("Portfolio","date","Type")]
# 
# CarryDataTotal=cast(CarryDataTotal,date ~ Portfolio, fun.aggregate = mean,value="value")
# # preparing the returns dataset
# ReturnDataTotal=melt.data.table(RetunrnData_ZeroNARep,id="date")
# setkey(ReturnDataTotal,variable)
# ReturnDataTotal_= Assets[ReturnDataTotal,allow.cartesian = TRUE]
# 
# # final db for data base analysis
# ReturnDataTotal_=ReturnDataTotal_[,list(value=sum(value*Weight)),
#                                   by=c("Type","date","Portfolio","Rk")]
# 
# # returns data to run the optimization
# RetDataTotal=cast(ReturnDataTotal_,date ~ Portfolio, fun.aggregate = mean,value="value")
# 
# 
# # organizing the data set
# setcolorder(CarryDataTotal, colnames(RetDataTotal))
# CarryDataTotal$date=as.Date(CarryDataTotal$date)
# # weekly data
# ddates = CarryDataTotal$date
# 
# CarryDataTotal[] <- lapply(CarryDataTotal, function(x) as.numeric(as.character(x)))
# CarryDataTotal$date=ddates
# 
# CarryDataTotal=as.data.frame(CarryDataTotal)

# ref to the mmkt data
CarryDataTotal = mmkt_assets_carry
RetDataTotal = mmkt_assets_return
# Till here necessary to run the last chapter ####
dataref = as.Date("2023-05-26")
dataref_max = Sys.Date()
dataref_min = as.Date("2013-12-01")


#date to ref for the returns data
dataref_Adj= dataref #datarefmin(nextweekday(dataref,6),max(CarryDataTotal$date))

#reference volatility before the ref date moneymkt
sampleret2=mmkt_assets_return[date>dataref_min]

#1.5 Picking the return Matrix ####
sampleret_riskmx=sampleret2[complete.cases(sampleret2)]
 
### 1.5 Stats on carry and returns
###1.5.1 Carry Analysis ####
CarryChangeStats = as.data.table(melt(CarryDataTotal,id="date"))
CarryChangeStats$YY=format(CarryChangeStats$date,"%Y")
CarryChangeStats2=CarryChangeStats[,list(date,value,YY,VarZ=value - c(NA,value[-.N])),
                                   by=c("variable")]
CarryChangeStats3=CarryChangeStats2[,list(date,value,VarZ,
                                          YTD_Var_Bps=round(cumsum(ifelse(is.na(VarZ),0,VarZ))*100,2),
                                          YTD_Max=round(cummax(ifelse(is.na(value),0,value)),2),
                                          YTD_Min=round(cummin(ifelse(is.na(value),0,value)),2)),
                                   by=c("variable","YY")]
CarryChangeStats4=CarryChangeStats3[,list(date,value,VarZ,
                                          YTD_Var_Bps,
                                          YTD_Max,
                                          YTD_Min,
                                          YY,
                                          M3_VarZ_Bps=round(rollapply(VarZ, 12, sum,  by = 1,
                                                            fill = NA,align = "right")*100,2),
                                          M3_Min=round(rollapply(value, 12, min,  by = 1,
                                                                  fill = NA,align = "right"),2),
                                          M3_Max=round(rollapply(value, 12, max,  by = 1,
                                                            fill = NA,align = "right"),2)),
                                    by=c("variable")]



setkey(CarryChangeStats4,variable,date,YY)



###1.5.2 Volatility Analysis ####
VolDb = as.data.table(melt(RetDataTotal,id="date"))
VolDb.1= VolDb[,list(date,DailyR=value,
                     M3_RelVol=rollapply(value, 66, sd,  by = 1, fill = NA,align = "right",
                                         na.rm=TRUE)*sqrt(252)),
               by="variable"]
VolDb.1$YY=format(VolDb.1$date,"%Y")
VolDb.1[, VolVar:= M3_RelVol-c(NA,M3_RelVol[-.N]), 
        by="variable"]
VolDb.2 = VolDb.1[,list(date,DailyR,M3_RelVol,VolVar,VolVarYTD=cumsum(ifelse(is.na(VolVar),0,VolVar))),
                  by=c("variable","YY")]

setkey(VolDb.2,variable,date,YY)

##1.5.3 Aggregate Data for visualization ####
dd_plot=dataref_Adj

DataOvrallStats = VolDb.2[CarryChangeStats4]
RefDb = as.data.frame(DataOvrallStats[date %in% c(dd_plot)])

minn=min(RefDb$value)
maxn=max(RefDb$value)
mid=minn+(maxn-minn)/2

minn1=min(RefDb$M3_RelVol,na.rm=T)
maxn1=max(RefDb$M3_RelVol,na.rm=T)
#mid=(maxn-minn)/2-5


dev.off(dev.list()["RStudioGD"])
ggplot(data=subset(RefDb,date==c(dd_plot)), 
       aes(x=VolVarYTD, y=YTD_Var_Bps)) +
  geom_point(aes(colour = value,size=M3_RelVol)) +
  # geom_point(data=optpoint, aes(x=Std.Dev, y=Exp.Return),
  #            color=ealred, size=6) +
  #geom_smooth( method=lm, se=FALSE)+
  geom_text_repel(aes(x=VolVarYTD, y=YTD_Var_Bps,label=paste(variable,round(value,1),sep=",")),
                  family = "Bahnschrift",max.overlaps = 15)+
  ylab("Carry Change in Bps")+
  xlab("Volatility Change in % Points")+
  scale_color_gradient2(low = "blue", mid = "white", 
                        high = "red", midpoint = 0,
                        limits = c(minn, maxn), name="Carry")+
  scale_size_continuous(range = c(minn1, maxn1),
             breaks = round(seq(minn1,maxn1,10),0),name = "Realized Volatility")+
  theme(panel.background=element_rect(fill="white"),
        panel.grid.major =element_line(colour = "gray85",linetype = "dashed"),
        text=element_text(size=12,color="black"),
        plot.title=element_text(size=20, color="darkred"))+
  theme(axis.text.y = element_text(color = "black", size = 24, family = "Bahnschrift"),
        axis.text.x = element_text(color = "black", size = 24, family = "Bahnschrift"),
        axis.title.x = element_text(color = "black", size = 24, family = "Bahnschrift"),
        axis.title.y = element_text(color = "black", size = 24, family = "Bahnschrift"))+
  theme(legend.position = "bottom",
        legend.text = element_text(family = "Bahnschrift", size = 12),
        legend.title = element_text(family = "Bahnschrift", size = 12))+
  geom_hline(yintercept=0,linetype="dashed", color="black")+
  geom_text(aes(x=1.2,y=2),label=dd_plot,
            family = "Bahnschrift",size=4)


#2.1 Create db for Optimization of strategy ####
#last carry figures
samplecarry_post_covid = as.data.table(CarryDataTotal)[date == dd_plot]

# # specific selection based on rates curves
# Cuvedt=as.data.table(CarryDataTotal)[US_2s10s < 0]
# CUrve2s10s_neg=Cuvedt$date
# #period in which the curve reached a cycle low
# Cycle1= seq.Date(as.Date("2000-04-28"),as.Date("2000-04-28")+365,by="1 day")
# Cycle2= seq.Date(as.Date("2006-11-24"),as.Date("2006-11-24")+365,by="1 day")
# Cycle3= seq.Date(as.Date("2019-08-30"),as.Date("2019-08-30")+365,by="1 day")

Stats4_= CarryChangeStats4[date == dataref]
setkey(Stats4_,variable)

Stats4_$date=NULL
Stats4_$value=NULL
reference_ret=as.data.table(RetDataTotal)
reference_ret$date = NULL

# 3 different return sets
#reference_ret_cycleflat=as.data.table(RetDataTotal)[date %in% c(Cycle1,Cycle2,Cycle3),]
reference_ret_global=as.data.table(RetDataTotal)
#reference_ret_recent=as.data.table(RetDataTotal)[date > as.Date("2020-12-31"),]

dateset=reference_ret_global$date

# Correlation plot analisis
assetSelection = colnames(reference_ret_global)[-1]
nassets=length(assetSelection)
#date %in% seq.Date(as.Date("2013-06-01"),as.Date("2014-06-01"),by="1 day")
ref_selection=reference_ret_global[,assetSelection,with=FALSE]
date_selection=reference_ret_global[]$date

getwd()
checkperf=as.xts(as.matrix(ref_selection[,]/100),order.by =as.Date(date_selection))
charts.PerformanceSummary(checkperf[,])
cormatrix=cor(ref_selection)
numsize=ifelse(nassets<6,1.5,1)
textsize=ifelse(nassets<6,1.25,1)
pals=colorRampPalette(c("darkblue","white","darkred"))(10)


colnames(cormatrix) = mmkt_assets_des$Index[match(colnames(cormatrix),mmkt_assets_des$Code)]
rownames(cormatrix) = colnames(cormatrix)
path="C:/Users/eusea/R_charts_output"
png(height=20, width=20, file=paste(path,"HistCorrelation_Contrib_LongTerm.png",sep="/"), type = "cairo-png",
    units = "cm",res=1000)
par(family="Calibri",mar = rep(0,4),oma=rep(0,4))
# corrplot.mixed(cormatrix,  tl.col = "black", lower.col = pals,
#                number.cex = numsize,diag = FALSE,
#                cl.cex = textsize,tl.cex = textsize, 
#                upper.col = pals)
corrplot(cormatrix,  tl.col = "black", order = 'hclust', addrect = 4,
               number.cex = numsize,diag = FALSE,col = pals,
               cl.cex = textsize,tl.cex = textsize)
dev.off()


#rolling correlation 
# ut <- upper.tri(cormatrix)
# n <- paste(rownames(cormatrix)[row(cormatrix)[ut]],rownames(cormatrix)[col(cormatrix)[ut]])
# dt_set=ref_selection
# 
# rollingcorr.1y <- rollapply(dt_set,
#                             width=252,
#                             FUN = function(Z)
#                             {
#                               return(cor(Z,use="pairwise.complete.obs")[ut])
#                             },
#                             by.column=FALSE, align="right")
# 
# colnames(rollingcorr.1y) <- n
# relevant_dates=date_selection[252:length(date_selection)]
# dset_summary=summary(rollingcorr.1y)
# 
# rollingcorr.1y.df=data.frame(rollingcorr.1y)
# rollingcorr.1y.df$date=relevant_dates
# 
# rollingcorr.1y.melt <- melt(rollingcorr.1y.df,id="date")
# 
# corr_evo=ggplot(rollingcorr.1y.melt,aes(x=date)) +
#   geom_area(aes(y=value)) +
#   facet_grid(variable~.) +
#   ylim(c(-1,1)) +
#   scale_colour_economist()+
#   MainChart+
#   theme(axis.text.y = element_text(color = "black", size = 15, family = "Bahnschrift",hjust = 1),
#       axis.text.x = element_text(color = "black", size = 15, family = "Bahnschrift",vjust = 1, angle = 0),
#       axis.title.y = element_blank(),
#       axis.title.x = element_blank())+
#   theme(strip.text.y = element_text(size = 13, angle = 90))
# getwd()
# ggsave(paste(path,"CorrelationEvo",".png",sep=""), plot = corr_evo, device = png,
#        scale = 2, width = 17, height = 10, units = c("cm"),
#        dpi = 600)


# # generate 3 correlation matrix set
# cormatrix_LT=cormatrix
# cormatrix_stressed=cormatrix
# cormatrix_mild=cormatrix
# 
# vol_assets=apply(ref_selection,2,sd)
# expret_assets=samplecarry_post_covid[,assetSelection,with=FALSE]/100/252
# 
# stress=apply(rollingcorr.1y,2,max)
# mild=apply(rollingcorr.1y,2,min)
# 
# cormatrix_stressed[2,1]=stress[1]
# cormatrix_stressed[1,2]=stress[1]
# cormatrix_stressed[3,1]=stress[2]
# cormatrix_stressed[1,3]=stress[2]
# cormatrix_stressed[3,2]=stress[3]
# cormatrix_stressed[2,3]=stress[3] 
# 
# 
# cormatrix_mild[2,1]=mild[1]
# cormatrix_mild[1,2]=mild[1]
# cormatrix_mild[3,1]=mild[2]
# cormatrix_mild[1,3]=mild[2]
# cormatrix_mild[3,2]=mild[3]
# cormatrix_mild[2,3]=mild[3] 
# 
# # plotting the stress and mild correlation matrix
# 
# path=paste(getwd(),"/output",sep="")
# png(height=18, width=18, file=paste(path,"HistCorrelation_Stressed.png",sep="/"), type = "cairo-png",
#     units = "cm",res=1000)
# par(family="Bahnschrift",mar = rep(1,4),oma=rep(0,4))
# corrplot.mixed(cormatrix_stressed,  tl.col = "black", lower.col = "black",number.cex = numsize,
#                cl.cex = textsize,tl.cex = textsize, 
#                upper.col =colorRampPalette(c("darkblue","white","darkred"))(100))
# dev.off()
# 
# path="C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data"
# png(height=18, width=18, file=paste(path,"HistCorrelation_mild.png",sep="/"), type = "cairo-png",
#     units = "cm",res=1000)
# par(family="Bahnschrift",mar = rep(1,4),oma=rep(0,4))
# corrplot.mixed(cormatrix_mild,  tl.col = "black", lower.col = "black",number.cex = numsize,
#                cl.cex = textsize,tl.cex = textsize, 
#                upper.col =colorRampPalette(c("darkblue","white","darkred"))(100))
# dev.off()
# 
# # creating the covariance matrix
# new_varcov_stress=cor2cov(cormatrix_stressed,vol_assets)
# new_varcov_stress_pos=nearPD(new_varcov_stress)
# new_varcov_stress_mx=as.matrix(new_varcov_stress_pos$mat)
# 
# new_varcov_mild=cor2cov(cormatrix_mild,vol_assets)
# new_varcov_mild_pos=nearPD(new_varcov_mild)
# new_varcov_mild_mx=as.matrix(new_varcov_mild_pos$mat)
# 
# 
# Drift <- expret_assets
# Vol <- vol_assets
# nass=length(Vol)
# n <- 5366
# z_stress <- rmvnorm(n, sigma=new_varcov_stress_mx, method="chol")
# z_mild <- rmvnorm(n, sigma=new_varcov_mild_mx, method="chol")
# 
# GBM_Stress <- z_stress + matrix(as.numeric(Drift),n/nass,nrow=n,ncol = nass)
# colnames(GBM_Stress)=names(vol_assets)
# GBM_mild <- z_mild + matrix(as.numeric(Drift),n/nass,nrow=n,ncol = nass)
# colnames(GBM_mild)=names(vol_assets)
# 
# # Return matrix selection #

selected_rmx=RetDataTotal
selected_rmx$date = NULL

#selecting the data
samplecarry_post_covid$date = NULL


# running the optimization
samplecarry=t(as.matrix(samplecarry_post_covid))

## selection of the righit matrix ##
srNames=colnames(selected_rmx)[-match("CB41",colnames(selected_rmx))]
selected_rmx=selected_rmx[,..srNames]
## 

samplecarry=as.matrix(samplecarry[match(srNames,rownames(samplecarry)),])

samplecarryDf= data.frame(expRet=as.numeric(samplecarry),
                          variable=srNames,
                          Vol=apply(selected_rmx, 2, sd)*sqrt(252))

# set weight limits 

wgt_limup = c(rep(1,length(colnames(selected_rmx))))
wgt_limdown = c(rep(0,length(colnames(selected_rmx))))

w_max=unique(wgt_limup)
w_min=unique(wgt_limdown)

w_gap=(w_max-w_min)/4

names(wgt_limup)=colnames(selected_rmx)
names(wgt_limdown)=colnames(selected_rmx)

samplecarryDf=as.data.table(samplecarryDf)
setkey(samplecarryDf,variable)

#2.1 Design frontier EfficientFrontier ####

Select_Asset_Full = srNames
sampleret_EF=as.matrix(selected_rmx)

d_data_fullset=eff.frontier_weight(returns=sampleret_EF,selection=Select_Asset_Full,
                                   exp_ret=samplecarry,short="yes",
                                   max.allocation=wgt_limup[Select_Asset_Full],
                                   min.allocation=wgt_limdown[Select_Asset_Full],
                                   risk.premium.up=2,
                                   risk.increment=0.00005,
                                   frequency=252)


optpoint = d_data_fullset[d_data_fullset$sharpe==max(d_data_fullset$sharpe),]
optpoint6pct = d_data_fullset[round(d_data_fullset$Exp.Return,2)==7.01,]

optpoint = optpoint
# save the three set
# stress_front=d_data_fullset
# stress_front$mx_ref="stress"
# stress_flat_opt=optpoint
# stress_flat_opt$mx_ref="stress"
global_front=d_data_fullset
global_opt=optpoint
global_front$mx_ref="global"
global_opt$mx_ref="global"


# mild_front=d_data_fullset
# mild_opt=d_data_fullset
# mild_front$mx_ref="mild"
# mild_opt$mx_ref="mild"


frontierDB=global_front
optpointDB=global_opt
  
#load("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/optpointDB3Scen.rda")  
#load("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/Frontier3Scen.rda")
# save(frontierDB, file = "C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/Frontier3Scen.rda")
# save(optpointDB, file = "C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data/optpointDB3Scen.rda")


AdditionalStats=Stats4_[samplecarryDf]

optpointDB_melt=as.data.table(melt(optpointDB, id=c("Exp.Return","sharpe","Std.Dev","VaR20Days","mx_ref")))

setkey(AdditionalStats,variable)
setkey(optpointDB_melt,variable)
AdditionalStats2=optpointDB_melt[AdditionalStats]

# AdditionalStats2$Vol_Adj=AdditionalStats2$Vol*AdditionalStats2$ES

max1=round(max(AdditionalStats2$value),2)+0.01
min1=round(min(AdditionalStats2$value),2)

mid= min1+(max1 -min1)/2

ymin=min(0,floor(min(frontierDB$Exp.Return,AdditionalStats$expRet)))
maxvol=max(AdditionalStats$Vol)
mindiff=min(abs(frontierDB$Std.Dev-maxvol))

ymax=10

y_gap=(ymax-ymin)/5
y_breaks=seq(ymin,ymax,y_gap)


xmin=floor(min(0,d_data_fullset$Std.Dev,AdditionalStats$Vol))
xmax=7.5
x_gap=(xmax-xmin)/5
x_breaks=seq(xmin,xmax,x_gap)

setkey(AdditionalStats2,variable)
setnames(mmkt_assets_des,"Code","variable")
setkey(mmkt_assets_des,variable)
AdditionalStats2=AdditionalStats2[mmkt_assets_des]


OpportunitySet=ggplot() +
  geom_line(data=frontierDB, aes(x=Std.Dev, y=Exp.Return,color=mx_ref),
            alpha=1,linewidth=1,linetype="solid") +
  # geom_line(data=d_data_fullset_no_FX, aes(x=Std.Dev, y=Exp.Return),
  #            alpha=0.7,size=1,color="gray",linetype="dashed") +
  # geom_point(data=optpoint, aes(x=Std.Dev, y=Exp.Return),
  #            fill="black", size=4,shape=23) +
  geom_point(data=optpointDB, aes(x=Std.Dev, y=Exp.Return,fill=mx_ref),
             size=8,shape=23) +
  geom_point(data=optpoint6pct, aes(x=Std.Dev, y=Exp.Return,fill="6% Percent Target"),
             size=8,shape=23) +
  guides(fill=guide_legend(title="Regime"),
         color=guide_legend(title="Regime"))+
  # geom_hline(yintercept=optpoint$Exp.Return,linetype="dashed", color="black")+
  # geom_vline(xintercept =optpoint$Std.Dev ,linetype="dashed",color="black")+
  # geom_hline(yintercept =optpoint_no_FX$Exp.Return ,linetype="dashed",color="black")+
  geom_point(data=AdditionalStats2[!is.na(value)& mx_ref  == "global"], aes(x=Vol, y=expRet),size=4,color="gray") +
  geom_text_repel(data=AdditionalStats2[!is.na(value) & mx_ref  == "global"],aes(x=Vol, y=expRet,label=Index), family="Calibri",
                  max.overlaps = 50,size=5)+
  # scale_color_gradient2(low = "red", mid = "white", 
  #                       high = "darkblue", midpoint = 0,
  #                       limits = c(mean(wgt_limdown), max1),breaks=seq(w_min,w_max,w_gap),
  #                       labels=seq(w_min,w_max,w_gap)*100,name="Alloc % tangency\nPorfolio")+
  theme(panel.background=element_rect(fill="white"),
        panel.grid.major =element_line(colour = "gray85",linetype = "dotted"),
        text=element_text(size=25),
        plot.title=element_text(size=20, color="gray"))+
  theme(axis.text.y = element_text(color = "black", size = 24, family = "Calibri"),
        axis.text.x = element_text(color = "black", size = 24, family = "Calibri"),
        axis.title.x = element_text(color = "black", size = 24, family = "Calibri"),
        axis.title.y = element_text(color = "black", size = 24, family = "Calibri"))+
  theme(legend.position = "bottom",
        legend.text = element_text(family = "Calibri", size = 15),
        legend.title = element_blank())+
  #guides(guide_legend(title="Alloc %"))+
  geom_hline(yintercept=0,linetype="dotted", color="black")+
  geom_vline(xintercept =0 ,linetype="dotted",color="black")+
  scale_y_continuous(breaks = y_breaks,
                     limits = c(ymin, ymax)) +
  scale_x_continuous(breaks = x_breaks,
                     limits = c(xmin, xmax)) +
  scale_fill_economist(label = c("7% Target Return","Max Sharpe Portfolio",
                                 "Stress"))+
  scale_colour_economist(label = c("Efficient Frontier"))+
  #ggtitle("Efficient Frontier\nand Optimal Portfolio") +
  labs(x="Expected Volatility %", y="Expected Return %")


getwd()
ggsave(paste(path,"/opportunitySet_ScenarioS",".png",sep=""), plot = OpportunitySet, device = png,
       scale = 2, width = 20, height = 12, units = c("cm"),
       dpi = 600)
getwd()
#2.2.1 Analysis of the Allocation ####

Alloc1 = as.data.table(melt(optpointDB,id=c("Std.Dev","Exp.Return","sharpe","VaR20Days","mx_ref")))
setnames(Alloc1,"variable","Portfolio")
setkey(Alloc1,Portfolio)
Alloc1=Portfolio[Alloc1]

Alloc1$value =Alloc1$value*100

USDweight=-sum(Alloc1[Type=="FX_Carry"]$value)

AllocMx=Alloc1[,list(Portfolio,value,mx_ref)]
AllocNew=rbind()

p<-ggplot(data=AllocMx, aes(x=Portfolio, y=value,fill=mx_ref)) +
  geom_bar(stat="identity",position = "dodge")+
  #facet_wrap(reorder(AssetBloc,-nchar(AssetType)) ~.,scales = "free") +
  ylab("Weight %")+
  scale_y_continuous(breaks = seq(-100,100,25),
                     limits = c(-101, 101))+
  theme(legend.position = "bottom",
        legend.text = element_text(family = "Bahnschrift", size = 15),
        legend.title = element_blank())+
  scale_fill_economist()+
  MainChart+
  theme(axis.text.y = element_text(color = "black", size = 15, family = "Bahnschrift",hjust = 1),
        axis.text.x = element_text(color = "black", size = 15, family = "Bahnschrift",vjust = 1, angle = 0),
        axis.title.y = element_text(color = "black", size = 15, family = "Bahnschrift",angle = 90),
        axis.title.x = element_blank())
  
setwd("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Asset Allocation/Data")
ggsave(paste("optimal_set_Scenarios",".png",sep=""), plot = p, device = png,
       scale = 2, width = 17, height = 10, units = c("cm"),
       dpi = 600)


#3.1 Black Litterman Transformation of the expected returns ####
# views set-up 

pick <- newPMatrix(assetSelection[order(assetSelection)], 2)

pick[1,1] <- 1
pick[2,2] <- -1
pick[2,3] <- 1


# views volatility
# pick the recent matrix to compute the view 
returnMtrix=GBM_Stress[,colnames(pick)]*100
cormatrix=cor(returnMtrix)
covmatrix=cov(returnMtrix)
volasset=apply(returnMtrix,2,stdev)
volasset_ann=apply(returnMtrix,2,stdev)*sqrt(252)

# volatility of the views 
vol=apply(returnMtrix%*%t(pick),2,stdev)
vol_ann=apply(returnMtrix%*%t(pick),2,stdev)*sqrt(252)
vol_ann/sqrt(12)
conf <- c(0.75,0.6)*100
horizon = sqrt(252)
expected_ret_views= vol*horizon

myViews <- BLViews(pick, q = expected_ret_views, confidences = conf, assetSelection)

#checking varcov consistency
varcov=cov(returnMtrix)

#new varcov
corr_gen_diag=diag(volasset)^2
cormatrix_2=cormatrix

cormatrix_2[3,2]=0.8
cormatrix_2[2,3]=0.8
#with new correaltion-volatility embedded  
new_varcov=cor2cov(cormatrix_2,volasset)

#expected returns
mu=samplecarry[colnames(pick),]
#tau = is the key driver of the view the close to one the less certain I am on the view
# assumption 
posterior <- posteriorEst(myViews, tau = 0.25, mu=mu, sigma=varcov)
PostReturns=posterior@posteriorMean
PostReturns.dt=as.data.table(melt(PostReturns))
PostReturns.dt$Portfolio=names(PostReturns)
setkey(PostReturns.dt,Portfolio)
setkey(asset_db_last,Portfolio)
newret=asset_db_last[PostReturns.dt]




# plot the new portfolio 
carry_post=ggplot(data=newret,
             aes(x=reorder(Portfolio,-Rk),y=ifelse(Type=="FX_Carry",-i.value,i.value),
                 fill=Portfolio,label=ifelse(Type=="FX_Carry",-round(i.value,2),round(i.value,2))))+
  geom_bar(stat='identity')+
  scale_fill_manual(values = c("coral","lightblue","lightgreen"))+
  geom_text(size = 3, check_overlap = TRUE, position = position_stack(vjust = 0.5),
            family="Bahnschrift")+
  theme(panel.background=element_rect(fill="white"),
        panel.grid.major =element_line(colour = "gray85",linetype = "dashed"),
        #text=element_text(size=12,color="black"),
        plot.title=element_text(size=20, color="darkred"))+
  theme(axis.text.y = element_text(color = "black", size = 10, family = "Bahnschrift"),
        axis.text.x = element_text(color = "black", size = 14, family = "Bahnschrift"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text=element_text(size=8,family = "Bahnschrift",color = "White"))+
  theme(legend.position = "bottom",
        legend.text = element_text(family = "Bahnschrift", size = 10),
        legend.title = element_blank())+
  geom_hline(yintercept=0,linetype="dashed", color="black")+
  coord_flip()

ggsave(paste("carryUSD_post",".png",sep=""), plot = carry_post, device = png,
       scale = 1, width = 12, height = 18, units = c("cm"),
       dpi = 100)


#

TargetPortfolio=optimalPortfolios.fPort(posterior,optimizer = "tangencyPortfolio",
                        constraints = c("maxW[1] = 1","minW[1] = -1",
                                        "maxW[2] = 1","minW[2] = -1",
                                        "maxW[3] = 1","minW[3] = -1"))


marginalContr= (ptfwgt %*% varcov / TargetPortfolio$posteriorOptimPortfolio@portfolio@portfolio$targetRisk["Sigma"])*0.01*sqrt(252)
totalContr=(ptfwgt %*% varcov / TargetPortfolio$posteriorOptimPortfolio@portfolio@portfolio$targetRisk["Sigma"])*ptfwgt*sqrt(252)
sum(totalContr)
#factor to scale the tangency porfolio 
riskSentiment= 1
ptfwgt=TargetPortfolio$posteriorOptimPortfolio@portfolio@portfolio$weights
# ptfwgt["USD_G10"]=-0.5
# ptfwgt["USD_LatAm"]=0.5
#risk estimate
ptfrisk=TargetPortfolio$posteriorOptimPortfolio@portfolio@portfolio$targetRisk["Sigma"]*sqrt(252)*riskSentiment
ptfrisk_monhtly=TargetPortfolio$posteriorOptimPortfolio@portfolio@portfolio$targetRisk["Sigma"]*sqrt(252/12)*riskSentiment


PostPtfdt=data.table("Wgt"=ptfwgt*riskSentiment,
                   "variable"=names(ptfwgt))
#chart
PostPtfWeight=PostPtfdt[,list(Portfolio=variable,value=Wgt*100/riskSentiment,mx_ref="views_included")]
AllocNew=rbind(AllocMx,PostPtfWeight)
pp=ggplot(data=AllocNew, aes(x=Portfolio, y=value,fill=mx_ref)) +
  geom_bar(stat="identity",position = "dodge")+
  #facet_wrap(reorder(AssetBloc,-nchar(AssetType)) ~.,scales = "free") +
  ylab("Weight %")+
  scale_y_continuous(breaks = seq(-100,100,25),
                     limits = c(-101, 101))+
  theme(legend.position = "bottom",
        legend.text = element_text(family = "Bahnschrift", size = 15),
        legend.title = element_blank())+
  MainChart+
  scale_fill_economist()+
  theme(axis.text.y = element_text(color = "black", size = 15, family = "Bahnschrift",hjust = 1),
        axis.text.x = element_text(color = "black", size = 15, family = "Bahnschrift",vjust = 1, angle = 0),
        axis.title.y = element_text(color = "black", size = 15, family = "Bahnschrift",angle = 90),
        axis.title.x = element_blank())

ggsave(paste("optimal_set_Scenarios_view",".png",sep=""), plot = pp, device = png,
       scale = 2, width = 17, height = 10, units = c("cm"),
       dpi = 600)


setkey(PostPtfdt,variable)
PostPtf=TargetPortfolio$posteriorOptimPortfolio@portfolio@portfolio$weights*riskSentiment

# data backtest
PostPtfExpectedRet=t(as.vector(PostReturns))%*%t(t(PostPtf))
PostPtfRet=returnMtrix%*%t(t(PostPtf))
PorfolioVol=sd(returnMtrix%*%t(t(PostPtf)))*sqrt(252)

hike1=as.Date("2000-05-31")
hike2=as.Date("2006-06-30")
hike3=as.Date("2018-12-15")

cut1=as.Date("2001-01-31")
cut2=as.Date("2007-10-31")
cut3=as.Date("2019-07-31")

recession1=as.Date("2001-04-01")
recession2=as.Date("2008-01-01")
recession3=as.Date("2020-03-01")

Eu_crisis=as.Date("2011-08-01")
span_dd=186


hist_retdata=as.data.table(RetDataTotal)#[date %in% Cycle2]#[date>(hike1-span_dd) & date<(hike1+span_dd),]
hist_ts=melt.data.table(hist_retdata,id="date")
setkey(hist_ts,variable)

hist_carrydata=asset_db_last[,list(variable=Portfolio,Carry=value)]
setkey(hist_carrydata,variable)

bkt_db=hist_ts[hist_carrydata][PostPtfdt]
bkt_db$WWYY=format(bkt_db$date,"%V-%Y")
bkt_db_weekly=bkt_db[,list(date=last(date),W_ret=sum(value*Wgt),Carry_ret=sum(Carry/252/100*Wgt)),
                     by=c("variable","WWYY")]

bkt_db_weekly[,CumRet:=cumprod(1+W_ret)-1,by="variable"]
bkt_db_weekly[,CumCarry:=cumprod(1+Carry_ret)-1,by="variable"]
bkt_db_weekly[,CumRetCarry:=CumRet+CumCarry,by="variable"]
#performance separate and cumulate performance

ggplot(bkt_db_weekly,
  aes(x = date, y = CumRet, colour = variable)) +
  geom_line()

ggplot(bkt_db_weekly,
       aes(x = date, y = CumCarry, colour = variable)) +
  geom_line()

#final chart

bkt_db_agg=bkt_db_weekly[,list(variable="AggPerf",CumRetCarryAgg=sum(CumRet+CumCarry),
                               date=last(date)),by="WWYY"]


ggplot() +
  geom_col(data=bkt_db_weekly,aes(x = date, y = CumRetCarry, fill = variable))+
  geom_line(data = bkt_db_agg,aes(date,CumRetCarryAgg,colour="AggPerf"),colour="darkblue",size=1)

# plotting all the components
bkt_db_agg1=melt.data.table(bkt_db_weekly[,list(date,variable,CumRet,CumCarry)],id=c("date","variable"))

ggplot() +
  geom_col(data=bkt_db_agg1,aes(x = date, y = value, fill = variable, alpha=variable.1))+
  geom_line(data = bkt_db_agg,aes(date,CumRetCarryAgg,colour="AggPerf"),colour="darkblue",size=1)+
  scale_alpha_manual(values = c(1,0.3))

# long term historical simulation
GBM_mild=data.frame(GBM_Stress)
GBM_mild$date=dateset
#as.data.table(GBM_mild)#
hist_retdata=as.data.table(RetDataTotal)#[date %in% Cycle2]#[date>(hike1-span_dd) & date<(hike1+span_dd),]

hist_ts=melt.data.table(hist_retdata,id="date")
setkey(hist_ts,variable)

hist_carrydata=asset_db_last[,list(variable=Portfolio,Carry=value)]
setkey(hist_carrydata,variable)

bkt_db=hist_ts[hist_carrydata][PostPtfdt]
bkt_db$WWYY=format(bkt_db$date,"%V-%Y")
bkt_db$MMYY=format(bkt_db$date,"%b%Y")
bkt_db$YY=format(bkt_db$date,"%Y")
bkt_db_weekly=bkt_db[,list(date=last(date),W_ret=sum(value*Wgt),Carry_ret=sum(Carry/252/100*Wgt)),
                     by=c("variable","WWYY")]
bkt_db_monhtly=bkt_db[,list(date=last(date),
                            M_ret=prod(1+sum(value*Wgt)-1),
                            Carry_ret=prod(1+sum(Carry/252/100*Wgt))-1,
                            Tot_ret=(prod(1+sum(value*Wgt))-1)+prod(1+sum(Carry/252/100*Wgt))-1),
                     by=c("variable","MMYY","YY")] 

bkt_db_yearly=bkt_db[,list(date=first(date),
                            M_ret=prod(1+sum(value*Wgt)-1),
                            Carry_ret=prod(1+sum(Carry/252/100*Wgt))-1),
                      by=c("variable","YY")]


bkt_db_monhtly_glb=bkt_db_monhtly[,list(date=last(date),
                            M_ret=sum(M_ret),
                            Carry_ret=sum(Carry_ret),
                            Total_return = sum(M_ret) + sum(Carry_ret)),
                      by=c("MMYY","YY")] 


bkt_db_monhtly_glb[,`:=`(CumRet=cumprod(1+Total_return),CumMax=cummax(cumprod(1+Total_return))),by="YY"]
bkt_db_monhtly_glb[, Ddown:= -(CumMax-CumRet)/CumMax,by="YY"]
bkt_db_monhtly_glb$MM=as.numeric(format(bkt_db_monhtly_glb$date,"%m"))


bkt_db_yearly_glb=bkt_db_yearly[,list(date=last(date),
                                        M_ret=sum(M_ret),
                                        Carry_ret=sum(Carry_ret),
                                        Tot_ret=sum(M_ret)+sum(Carry_ret)),
                                  by=c("YY")] 

# monthly data   
stop_loss=ptfrisk_monhtly
param_var=ptfrisk_monhtly*1.65
var95=VaR(bkt_db_monhtly_glb$M_ret,p=0.95)
var99=VaR(bkt_db_monhtly_glb$M_ret,p=0.99)

filterdates # adverse
filterdt2   #favour
mret=ggplot() +
  geom_col(data=bkt_db_monhtly_glb[date %in% filterdt2],aes(x = date, y =Total_return*100 ,
                                       fill = "Monthly Returns: Agg. Alloc."),width = 12)+
  #geom_line(data = bkt_db_monhtly_glb,aes(MM,(Ddown*100),colour=YY),size=1)+
  guides(colour = guide_legend(nrow = 2))+
  ylab("Returns %")+
  #ylab("DrawDown %")+
  geom_hline(yintercept = -param_var, linetype="dashed",color="darkred")+
  MainChart+
  scale_fill_economist()+
  #scale_x_continuous(breaks=c(1:12),limits = c(1,12))+
  theme(axis.text.y = element_text(color = "black", size = 15, family = "Bahnschrift",hjust = 1),
        axis.text.x = element_text(color = "black", size = 15, family = "Bahnschrift",vjust = 1, angle = 0),
        axis.title.y = element_text(color = "black", size = 15, family = "Bahnschrift",angle = 90),
        axis.title.x = element_blank())
getwd()
ggsave(paste("optimal_Porfolio_hist_Backtest_favour_Flat",".png",sep=""), plot = mret, device = png,
       scale = 2, width = 17, height = 10, units = c("cm"),
       dpi = 600)

# 12moths back testing  
bkt_db_montly_12mhist=bkt_db_monhtly[,list(date,MMYY,M_ret12m=rollapply(data = (1+M_ret), width =12, by=1, FUN = prod,fill=NA,align ="right",na.rm=F)-1,
                                          Carry_ret6m=rollapply(data = (1+Carry_ret), width = 12, by=1, FUN = prod,fill=NA,align ="right",na.rm=F)-1),
                                    by=c("variable")]

bkt_db_monthly_12mhist.m=melt.data.table(bkt_db_montly_12mhist,id=c("date","MMYY","variable"))

bkt_db_monthly_12mhist_agg=bkt_db_montly_12mhist[,list(variable="AggPerf12m",
                                                       CumRetCarryAgg=as.numeric(sum(M_ret12m+Carry_ret6m)),
                                                     date=last(date)),by="MMYY"]


BBGList=c("USYC2Y10 Index")
opt <- c("periodicitySelection" = "MONTHLY")
conn= blpConnect()
Data.Assets = bdh(BBGList,c("PX_LAST","CHG_NET_1D"),start.date = as.Date("1999-01-01"),
                  end.date =  Sys.Date(),options = opt,include.non.trading.days = FALSE,
                  returnAs = "xts")

setnames(bkt_db_yearly,"variable","Porfolio")
bkt_db_yearly_m=melt.data.table(bkt_db_yearly,id=c("Porfolio","YY","date"))

plot_bkt12m_split=ggplot() +
  geom_col(data=bkt_db_yearly_m[YY!="1999"],aes(x = date, y = value*100, fill=Porfolio,alpha=variable),width = 150)+
  geom_point(data = bkt_db_yearly_glb[YY!="1999"],aes(date,Tot_ret*100,colour="Agg. Return"),size=4)+
  geom_hline(yintercept = -1.65*ptfrisk_monhtly*sqrt(12), linetype="dashed",color="darkred")+
  scale_alpha_manual(values = c(1,0.1))+
  ylab("Monthly Returns")+
  MainChart+
  scale_fill_economist()+
  theme(axis.text.y = element_text(color = "black", size = 15, family = "Bahnschrift",hjust = 1),
        axis.text.x = element_text(color = "black", size = 15, family = "Bahnschrift",vjust = 1, angle = 0),
        axis.title.y = element_text(color = "black", size = 15, family = "Bahnschrift",angle = 90),
        axis.title.x = element_blank())+
  scale_colour_manual(values = c("red"),label="Agg. Return")+
  scale_y_continuous(breaks = seq(-30,30,5))

ggsave(paste("optimal_scenario_historical",".png",sep=""), plot = plot_bkt12m_split, device = png,
       scale = 2, width = 17, height = 10, units = c("cm"),
       dpi = 600)

bkt_db_monhtly_glb$total=(bkt_db_monhtly_glb$M_ret+bkt_db_monhtly_glb$Carry_ret)*100
filterdates=index(Data.Assets[Data.Assets[,"PX_LAST"]<(0),])
datafilter_dt=as.data.table(Data.Assets)
datafilter_dt[,lag_Ind:= c(NA,PX_LAST[-.N])]
filterdt2=datafilter_dt[lag_Ind<20]$index



plot_bkt_total=ggplot() +
  geom_col(data=bkt_db_monhtly_glb[][order(date)],aes(x = date, y = total, fill = "Agg. Monthly Ret."),width = 12)+
  geom_line(data = bkt_db_monthly_12mhist_agg,aes(date,CumRetCarryAgg*100,colour=variable),size=1)+
  geom_hline(yintercept = -param_var, linetype="dashed",color="darkred")+
  scale_alpha_manual(values = c(1,0.3))+
  scale_colour_manual(values = c("darkgreen"),label=c("Rolling 12M Performance"))+
  ylab("Monthly Returns")+
  MainChart+
  scale_fill_economist()+
  theme(axis.text.y = element_text(color = "black", size = 15, family = "Bahnschrift",hjust = 1),
        axis.text.x = element_text(color = "black", size = 15, family = "Bahnschrift",vjust = 1, angle = 0),
        axis.title.y = element_text(color = "black", size = 15, family = "Bahnschrift",angle = 90),
        axis.title.x = element_blank())+
  scale_y_continuous(breaks = seq(-30,30,2.5))+
  scale_x_date(seq(as.Date("2000-01-01"), as.Date("2022-12-31"),
      by="2 years"))

ggsave(paste("optimal_scenario_bkt_curve_StresCorr",".png",sep=""), plot = plot_bkt_total, device = png,
       scale = 2, width = 17, height = 10, units = c("cm"),
       dpi = 600)





#### 3.2 simulation of correlated returns ####
returnMtrix=sampleret_EF[,assetSelection]/100
cormatrix=cor(returnMtrix)
covmatrix=cov(returnMtrix)
volasset=apply(returnMtrix,2,sd)
volasset_ann=apply(returnMtrix,2,stdev)*sqrt(252)

#new varcov
corr_gen_diag=diag(volasset)^2
cormatrix_2=cormatrix

#corr latam vs g10
cormatrix_2[3,2]=0.8
cormatrix_2[2,3]=0.8

#corr US2_10 vs g10
cormatrix_2[2,1]=-0.25
cormatrix_2[1,2]=-0.25

#corr US2_10 vs latam
cormatrix_2[3,1]=-0.25
cormatrix_2[1,3]=-0.25


#with new correaltion-volatility embedded  
new_varcov=cor2cov(cormatrix_2,volasset)
new_varcov_pos=nearPD(new_varcov)
new_varcov_mx=as.matrix(new_varcov_pos$mat)

Covariance.matrix <- new_varcov
  Drift <- PostReturns/252/100
  Vol <- volasset
  n <- 100000
  z <- rmvnorm(n, sigma=new_varcov_mx, method="chol")

  apply(z,2,sd)
  
GBM <- Drift + z 

apply(GBM,2,sd)*100*sqrt(252)
cor(GBM)
#example 

sim = as.matrix(sampleret_EF[,assetSelection])/100
n=ncol(sim)
expret = t(t(PostReturns))
Target=5

#feasible starting values of equal weights
w = rep(1/n,n)



#### #optimization options####
opts <- list( "algorithm" = "NLOPT_LD_SLSQP",
              "xtol_rel" = 1.0e-10,
              "maxeval" = 1000)

#4.3 Running model on different target return ####
Rtgt=seq(2,10,0.5)
typeOpt=c("ES")
if("dt.output.tot" %in% ls()){rm(dt.output.tot)}
ubound=c(-0.5,1,1)
lbound=c(-1,-1,-1)
# Select the correlation maxtrix ####

for (jj in 1:length(typeOpt)){
 
  rm(dt.output)
  SelectedType=typeOpt[jj]
  
  for( i in 1:length(Rtgt)){
  
    
    nl = nloptr(w,toOpt,
                lb = rep(-1,n),
                ub = rep(1,n),
                eval_g_eq=eqCon,
                opts=opts,
                sim=sim,
                alpha=.01,
                TRet=Rtgt[i],
                exret=expret,
                period=252,
                type=SelectedType)
    
    s=nl$solution
    s1 = as.data.table(t(nl$solution))
    names(s1)=rownames(expret)
    
    TergetOut = TargetFun(s,exret=expret, sim = sim, type=SelectedType)
    # print(TergetOut)
    # print( s%*%expret)
    dt.output1 = s1
    
    dt.output1$ExReturn = s%*%expret
    dt.output1$RiskMeasure = if(SelectedType=="Skew"){-TergetOut}else{TergetOut}
    dt.output1$Type = SelectedType
    
    if("dt.output" %in% ls()){
      dt.output = rbind(dt.output,dt.output1)
    }else{
      dt.output = dt.output1
    }
    
  }
  
  
  if("dt.output.tot" %in% ls()){
    dt.output.tot = rbind(dt.output.tot,dt.output)
  }else{
    dt.output.tot = dt.output
    
    
  }
  
}


#### Plot results ####
SelectedType = "ES"

ggplot() +
  geom_point(data=dt.output.tot[Type==SelectedType], aes(x=RiskMeasure*100, y=ExReturn),
            alpha=1,size=2,color="Gray") +
  geom_path(data=dt.output.tot[Type==SelectedType], aes(x=RiskMeasure*100, y=ExReturn),
             color="Gray") +
  # geom_point(data=optpoint5pct, aes(x=Std.Dev, y=Exp.Return),
  #            color="blue", size=6) +
  # geom_point(data=optpoint, aes(x=Std.Dev, y=Exp.Return),
  #            color=ealred, size=6) +
  # geom_smooth(data=AdditionalStats2, aes(x= Vol, y=expRet,
  #                                        color= M3_VarZ),
  #             method=lm, se=FALSE)+
  # geom_pointrange(data=AdditionalStats2, aes(x=Vol, y=expRet,ymin=M3_Min, ymax=M3_Max,
  #                                            color= M3_VarZ),fatten = 6,size=0.5) +
  # geom_text_repel(data=AdditionalStats2,aes(x=Vol, y=expRet,label=variable))+
  # scale_color_gradient2(low = "red", mid = "yellow", 
  #                       high = "darkblue", midpoint = mid,
  #                       limits = c(min1, max1))+
  theme(panel.background=element_rect(fill="white"),
        panel.grid.major =element_line(colour = "gray85",linetype = "dashed"),
        text=element_text(size=12,color=text),
        plot.title=element_text(size=20, color="darkred"))+
  theme(axis.text.y = element_text(color = "black", size = 24, family = "Bahnschrift"),
        axis.text.x = element_text(color = "black", size = 24, family = "Bahnschrift"),
        axis.title.x = element_text(color = "black", size = 24, family = "Bahnschrift"),
        axis.title.y = element_text(color = "black", size = 24, family = "Bahnschrift"))+
  theme(legend.position = "bottom",
        legend.text = element_text(family = "Bahnschrift", size = 16),
        legend.title = element_text(family = "Bahnschrift", size = 12))+
  geom_hline(yintercept=0,linetype="dashed", color="black")+
  geom_vline(xintercept =0 ,linetype="dashed",color="black")+
  # scale_y_continuous(breaks = seq(-5,7.5,2.5),
  #                    limits = c(-5, 7.5)) +
  # scale_x_continuous(breaks = seq(0, 15, 5),
  #                    limits = c(0, 20)) +
  #ggtitle("Efficient Frontier\nand Optimal Portfolio") +
  labs(x=paste("Expected Daily %",SelectedType,sep=" "), y="Expected Return %")



#### Asset allocation evlution ##### 

EvolWeights = as.data.table(melt(dt.output.tot,id=c("RiskMeasure","ExReturn","Type")))
setkey(EvolWeights,variable)

AssetClassFilterRates= colnames(dt.output.tot)[c(25:33)]
AssetClassFilterCredit = c("Type","C0A0","ER00","H0A0","HE00")
AssetClassFilterFX = colnames(dt.output.tot)[c(5:24)]
ReturnFilter=c(2.50,5.00,7.50)

#radar plot data fromat
Template = as.data.table(dt.output.tot)
setcolorder(Template,c("Type",setdiff(names(Template), "Type")))
rowsname=Template[as.character(round(ExReturn,2))==5.00]$Type
Template1 = Template[as.character(round(ExReturn,2))==5.00,
                     AssetClassFilterRates,with=FALSE]
Template1$Type=NULL

row.names(Template1)=rowsname
#Template1$Type =NULL
Template1

limUp=ceiling(max(Template1)*10)/10
limDown=floor(min(Template1)*10)/10

df2 <- data.frame(sapply(Template1, function(x) as.numeric(as.character(x))))
row.names(df2)=row.names(Template1)

df3=rbind(rep(limUp,ncol(df2)) , rep(limDown,ncol(df2)),df2)
df3
centerzero = matrix(rep(0,ncol(df3)),nrow=1)
colnames(centerzero)=colnames(Template1)
row.names(centerzero)=paste("Target return","5%",sep=" ")
df3=rbind(df3,centerzero)

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9),"#000000",rgb(0.1,0.2,0.3,0.9))
colors_in=c( rgb(0.2,0.5,0.5,0.2), rgb(0.8,0.2,0.5,0.2) , rgb(0.7,0.5,0.1,0.2) ,rgb(0,0,0,0),rgb(0.1,0.2,0.3,0.2))


png("FX.png", width=8, height=8, units="in", res=600)
op=par(family = "Bahnschrift", font=4,pch=30)
radarchart(df3, axistype=1 ,
            #custom polygon
            pcol=colors_border , pfcol=colors_in,
            plwd=1, 
            plty=c(1,1,1,3),
            pty=c(20,20,20,NA),
            #custom the grid
            cglcol="grey", cglty=1, 
            axislabcol="grey",
            caxislabels=round(seq(limDown,limUp,abs(limDown-limUp)/4)*100,0),
            cglwd=0.8,
            #custom labels
            vlcex=1,
            pch=30)

legend(x=1, y=1, legend = row.names(df3)[-c(1,2)],
       bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

dev.off()
getwd()


#5.1  Evolution of the efficient Allocation  #####
opts2 <- list( "algorithm" = "NLOPT_LD_SLSQP",
              "xtol_rel" = 1.0e-10,
              "maxeval" = 10000)
blpConnect()
USDrate=as.data.table(bdh("US0003M Index","PX_LAST",as.Date("2000-01-01")))


for (span in c(12,26,52,104)){
  rm(dt.output)
  for (window in c(1,4,12,24)){
    rm(dt.output)
    for (ii in seq(span,nrow(CarryDataTotal),by=window)){
      dt_val=CarryDataTotal$date
      
      
      #making sure to find a right cash rate
      ref_cash_rate = USDrate[date==dt_val[ii]]$PX_LAST
      radj=1
      
      while(length(ref_cash_rate)==0){
        ref_cash_rate = USDrate[date==dt_val[ii-radj]]$PX_LAST
        radj = radj+1
      }
      
      if (ii==span){
        retdata=sampleret_riskmx[date<dt_val[ii]]
      }else{
        retdata=sampleret_riskmx[date<dt_val[ii] & date>dt_val[ii-span]]
      }
      
      
      rx_carry= as.data.table(CarryDataTotal)[date==dt_val[ii]]
      rx_carry$date=NULL
      rx_carry=t(as.matrix(rx_carry))
      
      
      rxMatrix=retdata
      rxMatrix$date=NULL
      rxMatrix=as.matrix(rxMatrix)*100
      
      nn=nrow(rx_carry)
      ww=rep(1/nn,nn)
      
      nl = nloptr(ww,toOpt,
                  lb = rep(-1,nn),
                  ub = rep(1,nn),
                  eval_g_eq=eqCon,
                  opts=opts2,
                  sim=rxMatrix,
                  alpha=.01,
                  TRet=ref_cash_rate*2,
                  exret=rx_carry,
                  type="DDown")
      
      s=nl$solution
      s1 = as.data.table(t(nl$solution))
      names(s1)=rownames(rx_carry)
      
      TergetOut = TargetFun(s,exret=rx_carry, sim = rxMatrix, type="DDown")
      dt.output1 = s1
      dt.output1$date=dt_val[ii]
      dt.output1$ExReturn = s%*%rx_carry
      dt.output1$RiskMeasure =TergetOut
      dt.output1$Type = "DDown"
      dt.output1$Window= window
      dt.output1$RefCashRate=ref_cash_rate
      
      if("dt.output" %in% ls()){
        dt.output = rbind(dt.output,dt.output1)
      }else{
        dt.output = dt.output1
      }
      
      print(ii)
      
    }
    
    save(dt.output, file = paste("HistWeights",format(Sys.Date(),"%Y%m%d"),"_Rebalancing_",window,"_TrainingPeriod_",span,".rda",sep=""))
    
  }
  
}

plott=melt(dt.output,id=c("date","ExReturn","Type","RiskMeasure"))

ggplot(plott[variable %in% c("USD","CHF")])+
  geom_line(aes(x=date,y=value,colour=variable))
getwd()

#### 5.3 Analyse results ####

for (span in c(1,4,12,24)){
  for (window in c(12,26,52,104)){
    
    destfile1=paste("HistWeights20181113_Rebalancing_",span,"_TrainingPeriod_",window,".rda",sep="")
    destfile2=paste("HistWeights20181112_Rebalancing_",span,"_TrainingPeriod_",window,".rda",sep="")
    
    
    if(file.exists(destfile1)){
      load(destfile1)
    }else{
      load(destfile2)
    }
    
    
    
    w_db=as.data.table(melt(dt.output,id=c("date","ExReturn","RiskMeasure","Type")))
    ret_db=as.data.table(melt(sampleret_riskmx,id=c("date")))
    carry_db=as.data.table(melt(CarryDataTotal,id=c("date")))
    
    setkey(w_db,date,variable)
    setkey(ret_db,date,variable)
    setkey(carry_db,date,variable)
    
    db_check=w_db[carry_db][ret_db]
    setnames(db_check,"value","wgt")
    setnames(db_check,"i.value","Carry")
    setnames(db_check,"i.value.1","D_Ret")
    
    ww=length(unique(carry_db$variable))
    
    db_backtest=db_check[,list(date,Carry=na.locf(na.locf(Carry,na.rm = F),fromLast=T),
                               wgt=na.locf(na.locf(wgt,na.rm = F),fromLast=T),
                               eq_weight=1/ww,
                               D_Ret,
                               D_Ret_tot=D_Ret+na.locf(na.locf(Carry,na.rm = F),fromLast=T)/100/252,
                               ExReturn=na.locf(na.locf(ExReturn,na.rm = F),fromLast = T),
                               RiskMeasure=na.locf(na.locf(RiskMeasure,na.rm = F),fromLast = T),
                               Type=na.locf(na.locf(Type,na.rm = F),fromLast = T)),
                         by="variable"]
    
    
    db_port_opt=as.data.table(melt(db_backtest[,list(Ptf_Quant=sum(wgt*D_Ret_tot),
                                                     Ptf_EqW=sum(eq_weight*D_Ret_tot),
                                                     Ptf_Quant_HitRatio=sum(ifelse(wgt*D_Ret_tot>0,1,0))/length(variable),
                                                     Ptf_EqW_HitRatio=sum(ifelse(eq_weight*D_Ret_tot>0,1,0))/length(variable)),
                                               by=c("date")],id="date"))
    
    db_port_opt2=db_port_opt[variable %in% c("Ptf_EqW","Ptf_Quant"),list(date,value,span=4,
                                                                         Cum_Perf=100*cumprod(1+ifelse(is.na(value),0,value))),
                             by="variable"]
    
    
    
    #
    db_port_opt$Year =format(db_port_opt$date,"%Y")
    db_port_opt$Semester =paste(ifelse(as.numeric(format(db_port_opt$date,"%m"))<=6,"1st","2nd"),format(db_port_opt$date,"%Y"),sep="-")
    
    blpConnect()
    USDrate=as.data.table(bdh("US0003M Index","PX_LAST",as.Date("2001-01-01")))
    setkey(USDrate,date)
    setkey(db_port_opt,date)
    
    
    db_port_opt1=BBSW3M[db_port_opt]
    
    db_port_opt3=db_port_opt1[variable %in% c("Ptf_EqW","Ptf_Quant"),list(date=mean(date),
                                                                          Cum_Perf=100*prod(1+ifelse(is.na(value),0,value))-100,
                                                                          CashRate=mean(PX_LAST,na.rm = T),
                                                                          Vol=(100*sd(value,na.rm =T)*sqrt(252)),
                                                                          SR_Relative=((100*prod(1+ifelse(is.na(value),0,value))-100)-mean(PX_LAST,na.rm = T))/(100*sd(value,na.rm =T)*sqrt(252)),
                                                                          SR_Absolute=((100*prod(1+ifelse(is.na(value),0,value))-100))/(100*sd(value,na.rm =T)*sqrt(252))),
                              by=c("variable","Year")]
    
    ggplot(db_port_opt2)+
      geom_line(aes(x=date,y=Cum_Perf,colour=variable),size=1)+
      geom_text_repel(data=db_port_opt2[date==max(date)],
                      aes(x=date,y=Cum_Perf,colour=variable,label=round(Cum_Perf,2)),
                      show.legend = FALSE,family="Bahnschrift",size=5)+
      theme(panel.background=element_rect(fill="white"),
            panel.grid.major =element_line(colour = "gray85",linetype = "dashed"),
            #text=element_text(size=12,color="black"),
            plot.title=element_text(size=20, color="darkred"))+
      scale_color_manual(values = c("darkblue","lightblue"))+
      theme(axis.text.y = element_text(color = "black", size = 18, family = "Bahnschrift"),
            axis.text.x = element_text(color = "black", size = 18, family = "Bahnschrift",angle=45),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            text = element_text(size = 10,family = "Bahnschrift"),
            axis.text=element_text(size=8,family = "Bahnschrift",color = "White"))+
      theme(legend.position = "bottom",
            legend.text = element_text(family = "Bahnschrift", size = 12),
            legend.title = element_blank())+
            guides(fill=FALSE)+
      geom_hline(yintercept=0,linetype="dashed", color="black")+
      scale_y_date(breaks = c(-2))
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")
      
    
    ggplot(db_port_opt3,aes(x=date,y=SR_Relative))+
      geom_bar(aes(fill=variable,colour=variable,group=variable),stat = "identity",
               position = position_dodge(width =  NULL))+
      geom_text(aes(group=variable,
                    label=round(SR_Relative,2)),show.legend = FALSE,family="Bahnschrift",size=4,
                position = position_dodge(width = 1))+
      theme(panel.background=element_rect(fill="white"),
            panel.grid.major =element_line(colour = "gray85",linetype = "dashed"),
            plot.title=element_text(size=20, color="darkred"))+
      scale_fill_manual(values = c("gray","lightblue"))+
      scale_color_manual(values = c("gray","lightblue"))+
      theme(axis.text.y = element_text(color = "black", size = 18, family = "Bahnschrift"),
            axis.text.x = element_text(color = "black", size = 18, family = "Bahnschrift",angle=45),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            text = element_text(size = 10,family = "Bahnschrift"),
            axis.text=element_text(size=8,family = "Bahnschrift",color = "White"))+
      theme(legend.position = "bottom",
            legend.text = element_text(family = "Bahnschrift", size = 12),
            legend.title = element_blank())+
      geom_hline(yintercept=0,linetype="dashed", color="black")+
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")
     
  
    
    }
}

load("HistWeights20181113_Rebalancing_4_TrainingPeriod_26.rda")




ggplot(db_HitRatio)+
  geom_line(aes(x=date,y=value,colour=variable))

#5.2 Backtesting the results  ####

TargetsDB=db_backtest[,list(date,RiskMeasure,ExReturn,Type)]
setkey(TargetsDB,date)

ExPostResults=db_port_opt2[variable %in% c("Ptf_Quant","Ptf_EqW")]
setkey(ExPostResults,date)


DB_CheckResults = TargetsDB[ExPostResults,allow.cartesian=TRUE]

