libs = c("tidyverse","rvest","XML","RCurl","rlist","stringi","ggplot2", "ggthemes",  "ggrepel","FactoMineR","corrplot",
         "data.table","quantmod","priceR","reshape","PerformanceAnalytics","factoextra","extrafont")
# install.packages("FactoMineR",lib="C:/Users/eusep/Documents/Rpacks")

.libPaths(c("C:/Users/eusep/Documents/Rpacks","C:/Program Files/R/R-4.2.0/library"))

lapply(libs, require, 
       lib.loc = c("C:/Users/eusep/Documents/Rpacks","C:/Program Files/R/R-4.2.0/library"),
       character.only = TRUE)

rm(list=ls())
source("C:/Users/eusep/OneDrive/Documenti/R/ChartLayout.R")
loadfonts(device = "win")
path="C:/Users/eusep/OneDrive/Documenti/Macro Framework/Database/"
#### Loading OldData ####
Specs=read.csv("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Database/REER_Specs.csv")

Data=as.data.table(melt(read.csv("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Database/REER_Index.csv"),id="X"))
Work_Data=Data[,list(dat_val=as.Date(X,"%d/%m/%Y"),
                     Country=variable,value,
                     Vol=sd(value/c(NA,value[-.N])-1,na.rm = T)),by="variable"]
options("digits"=6)                     
Work_Data2=Work_Data[,list(dat_val,
                     Country,value,Vol,
                     UpperBound=rollapply(data = (1+Vol), width = 3, by=1, FUN = prod, fill=NA,
                                          align ="right"),
                     LowerBound=rollapply(data = (1-Vol), width = 3, by=1, FUN = prod, fill=NA,
                                         align ="right"),
                     EffectiveBound=rollapply(data = (value/c(NA,value[-.N])), width = 12, by=1, FUN = prod, fill=NA,
                                          align ="right")),
                     by="variable"]

Work_Data2[,Factor:=ifelse(EffectiveBound>UpperBound,"Expensive",ifelse(EffectiveBound<LowerBound,"Cheap","Fair"))]


#### market data sensitivity #####
sens=new.env()

getSymbols(c("AUDUSD=X","EURUSD=X","GBPUSD=X","NZDUSD=X","USDNOK=X","USDSEK=X","USDCAD=X","USDJPY=X","USDCHF=X",
             "MXN=X","USDCOP=X","USDCLP=X","USDBRL=X",
             "USDPLN=X","USDCZK=X","USDHUF=X",
             "USDTRY=X","USDZAR=X","USDILS=X",
             "USDINR=X","USDIDR=X","USDTHB=X", "USDTWD=X","USDKRW=X",
             "IWDA.AS","DBC","SHY"),
           src='yahoo',env = sens, return.class="xts",from = '2006-05-16')


FXDes = as.data.table(read.csv("C:/Users/eusep/OneDrive/Documenti/Macro Framework/Database/FX_Specs.csv"))
setkey(FXDes,variable)

NOK= sens[["USDNOK=X"]]
pframe <- as.data.table(do.call(merge, lapply(sens,Cl)))
pframe <- pframe[complete.cases(pframe)]
colnames(pframe) <- gsub("MXN", "USDMXN", colnames(pframe))
colnames(pframe) <- gsub(".Close$", "", colnames(pframe))
colnames(pframe) <- gsub(".X$", "", colnames(pframe))
pframe.melt=melt.data.table(pframe,id=c("index","IWDA.AS","DBC","SHY"))
setkey(pframe.melt,variable)
pframe.melt1 = pframe.melt[FXDes]
pframe.melt1[, value_direct := value^Side, by="variable"]

pframe.melt3=pframe.melt1[,list(index,Daily_Ret=value_direct/c(NA,value_direct[-.N])-1,DBC=DBC/c(NA,DBC[-.N])-1,
                                              IWDA.AS=IWDA.AS/c(NA,IWDA.AS[-.N])-1, SHY=SHY/c(NA,SHY[-.N])-1),
                         by=c("variable","label")]
# setkey(pframe.melt2,variable)
# options("digits"=4) 
# pframe.melt3=pframe.melt2[FXDes]
# pframe.melt3$Daily_Ret=pframe.melt3$Daily_Ret*pframe.melt3$Side


dbset=gsub("=X","",c("AUDUSD=X","EURUSD=X","GBPUSD=X","NZDUSD=X","USDNOK=X","USDSEK=X","USDCAD=X","USDJPY=X","USDCHF=X"))
window=67

rm("totalout")
for (i in 1:9){
  
  data= pframe.melt3[variable==dbset[i]]
  nleght=nrow(data)
  data$variable=NULL 
  print(i)
  rm("ccyoutput")
  for(j in window:nleght){
     
      rolldset=data[(j-window+1):j,]
      dt_val=max(rolldset$index)
      rolldset$index=NULL 
      linear_m= summary(lm(Daily_Ret~SHY, rolldset))
      c_output=coef(linear_m)[,1]
      c_confidence=coef(linear_m)[,2]
      var=names(c_output)
      
      output=data.table(cross=rep(dbset[i],length(var)),date_val=rep(dt_val,length(var)),Expl=var,coeff=c_output,
                        upperB=c_output+c_confidence,lowerB=c_output-c_confidence)
      
      
      if (j==window){
        
        ccyoutput=output
        
      }else{
        
        ccyoutput=rbind(ccyoutput,output)
      
        }
      
      
    }
  
 
  if(i==1){
    
    totalout=ccyoutput
  }else{
    
    totalout=rbind(totalout,ccyoutput)
    
  }
  
  
   
}  
  
options("digits"=6) 
ggplot(totalout[Expl !="(Intercept)"& date_val > "2020-01-01"])+
  geom_line(aes(x=date_val,y=coeff,color=Expl))+
  geom_line(aes(x=date_val,y=upperB,color=Expl),linetype="dotted")+
  geom_line(aes(x=date_val,y=lowerB,color=Expl),linetype="dotted")+
  facet_wrap(~ cross, ncol=3,scale = "free")



##### PCA over a currency lot #####
RetMatrix=cast(pframe.melt3,index~label,fun.aggregate = mean,value="Daily_Ret")
RetM.xts=xts(RetMatrix[,-1],order.by=RetMatrix[,1])
colnames(RetM.xts)=colnames(RetMatrix[,-1])

EM=gsub("USD","",c("USDMXN",
"USDBRL",
"USDCOP",
"USDCLP",
"USDTRY",
"USDZAR",
"USDILS",
"USDINR",
"USDIDR",
"USDKRW",
"USDTWD",
"USDTHB",
"USDCZK",
"USDPLN",
"USDHUF"))

#save scree plot

G10=gsub("USD","",
         c("AUDUSD","EURUSD","GBPUSD","NZDUSD","USDJPY","USDNOK","USDSEK","USDCAD","USDCHF"))


res.pca <- PCA(RetM.xts["2024",G10], graph = T, scale.unit = T)
#dollar=cumprod(1+c(0,res.pca$ind$coord[,1]))

eig.val <- get_eigenvalue(res.pca)
pca.coord = res.pca$svd$V

var_expl=fviz_eig(res.pca, ylim = c(0, 80), ggtheme = theme_hc(),
         barfill = "darkblue",font.family = "Bahnschrift",
         barcolor = "darkblue",addlabels = F)+ 
  geom_text(label = round(res.pca$eig[,2],1), vjust=-0.4, hjust = 0, size = 8,family="Bahnschrift")+
  theme(text = element_text(family = "Bahnschrift",size=30),title = element_blank())

  
ggsave(paste(path,"Var_expl.png",sep=""), plot = var_expl, device = "png",
       scale = 2, width = 20, height = 10, dpi = 1000, 
       units = c("cm"))


fviz_pca_var(res.pca, col.var = "black")

head(res.pca$var$cos2, 4)
corrplot(res.pca$var$cos2, is.corr=FALSE)

#For a given variable, the sum of the cos2 on all the principal components is equal to one.
# If a variable is perfectly represented by only two principal components (Dim.1 & Dim.2),
# the sum of the cos2 on these two PCs is equal to one. 
# In this case the variables will be positioned on the circle of correlations.

fviz_cos2(res.pca, choice = "var", axes =1:2,addlabels = TRUE)


# contribution to the axis preferred measure in % #

#save contribution plot

#weight in each to the component 
png(height=5, width=5, file=paste(path,"Comp_Cooord.png"), type = "cairo-png",
    units = "in",res=500)
par(family="Bahnschrift")
corrplot(res.pca$var$coord, is.corr=FALSE,family="Bahnschrift",
         col = COL2('RdBu'),cl.cex = 0.7, cl.ratio=0.4,cl.pos = "r", tl.col="black" )  
dev.off()

#contribution to the component 
png(height=5, width=5, file=paste(path,"Comp_Contrib.png"), type = "cairo-png",
    units = "in",res=500)
par(family="Bahnschrift")
corrplot(res.pca$var$contrib , is.corr=FALSE,family="Bahnschrift",
         col = COL2('RdBu'),cl.cex = 0.7, cl.ratio=0.4,cl.pos = "r", tl.col="black" )  
dev.off()




# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)


fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)



res.km <- kmeans(res.pca$var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")
  

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)



PC1=data.table(DailyRet=as.matrix(res.pca$call$X)%*%(res.pca$var$coord[,1]),
               Variable="PC1",date=rownames(as.matrix(res.pca$call$X)))

PC1$Perf=100*cumprod(1+PC1$DailyRet.V1)
sens1=new.env()

getSymbols(c("UUP"),
           src='yahoo',env = sens1, return.class="xts",from = '2021-12-31')

Retframe <- as.data.table(do.call(merge, lapply(sens1,Cl)))

