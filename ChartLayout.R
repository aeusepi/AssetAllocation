#### Chart Layout ####
SignalChart = theme(text=element_text(size=16,  family="Bahnschrift"))+
  theme(axis.title.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y=element_blank(),axis.text.x = element_text(angle = 35,size=15),
        legend.title = element_blank(),panel.background = element_rect(fill = "grey90"))+
  theme(plot.caption = element_text(size=15))

MainChart =theme_economist()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),axis.text.x = element_text(angle = 35,size=15),
        legend.title = element_blank(),legend.position = "top",legend.text = element_text(size=15))+
  theme(plot.caption = element_text(size=15))+
  
  theme(text=element_text(size=15,  family="Bahnschrift"),
        panel.background = element_rect(fill = "white",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major.x = element_line(linewidth = 0.5, linetype = 'dotted',
                                          colour = "gray"), 
        panel.grid.major.y = element_line(linewidth = 0.5, linetype = 'dotted',
                                          colour = "gray"))

MainChart2 =theme_economist()+
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 35,size=15),
        legend.title = element_blank(),legend.position = "top",legend.text = element_text(size=15))+
  theme(plot.caption = element_text(size=15))+
  theme(text=element_text(size=10,  family="Bahnschrift"),
        panel.background = element_rect(fill = "white",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major.x = element_line(size = 0.5, linetype = 'dotted',
                                          colour = "gray"), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'dotted',
                                          colour = "gray"))  

MainChart3 =theme_economist()+
  theme(axis.title.x=element_text(size=15,margin = margin(t = 20)),
        axis.title.y=element_text(size=15,margin = margin(t = 10, r= 10)),
        axis.text.x = element_text(size=15,margin = margin(t = 0, b=0)),
        legend.title = element_blank(),legend.position = "top",legend.text = element_text(size=15))+
  theme(plot.caption = element_text(size=15))+
  theme(text=element_text(size=15,  family="Bahnschrift"),
        panel.background = element_rect(fill = "white",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major.x = element_line(linewidth = 0.5, linetype = 'dotted',
                                          colour = "gray"), 
        panel.grid.major.y = element_line(linewidth = 0.5, linetype = 'dotted',
                                          colour = "gray"))  

# chart directory directions # 
path= "C:/Users/eusep/OneDrive/Documenti/Macro Framework/Database/"


# functions
#rm(list=ls())
#"RCurl"
repl_out <- function(x, q1=0.05, q2=0.95){
  quantiles <- quantile( x, c(q1, q2) , na.rm=T)
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

# EMWA Volatilty function 

EMWA_Vol = function(x, lambda = 0.96, na.remove=T){
  
  
  avg= mean(x,na.rm=na.remove)
  ret_sq= (x-avg)^2
  wgts = matrix(data = NA,nrow=length(x),ncol=1)
  for(i in  1 : (length(x))){
    
    if (i==1){
      wgts[1,1] = (1-lambda)
    }else{
      
      wgts[i,1] = wgts[(i-1),1]*lambda
    }
    
  }
  
  wgts_=rev(wgts)
  vol= sqrt(sum(ret_sq*wgts_,na.rm = T))
  
  return(vol)
  
}


#### Supporting functions ####
#
maxddown = function(r){
  v <- cumprod(1 + ifelse(is.na(r),0,r))
  1 - v/cummax(v)         ## drawdown
  maxdd=max(1 - v/cummax(v))    ## max. drawdown
  return(maxdd)
}


ecdf_fun <- function(x){ 
  if (length(x)>=52){
    
    LV = last(x)
    result= ecdf(x)(last(x))
    
  }else{
    
    result = NA
  }
  
  return(result)
}

perc.rank=function(x){
  
  seg = sort(x)
  refind=which(seg==last(x)) 
  perc = refind/length(x)
  
  return(perc)
}

cor2cov <- function(R, S) {
  sweep(sweep(R, 1, S, "*"), 2, S, "*")
}

