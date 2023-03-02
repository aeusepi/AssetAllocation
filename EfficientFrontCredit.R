#### Efficient Frontier function ####
eff.frontier <- function (returns,exp_ret, short="no",
                          max.allocation=NULL,
                          min.allocation=NULL,
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
  
  
  
  if (is.positive.definite(cov(returns))){
    covariance <- cov(returns)
  }else{
    CovM=nearPD(cov(returns))
    covariance =CovM$mat
  }
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
  max.allocation=0.5
  min.allocation=-0.5
  # And modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(max.allocation)){
    if(max.allocation > 1 | max.allocation <0){
      stop("max.allocation must be greater than 0 and less than 1")
    }
    if(max.allocation * n < 1){
      stop("Need to set max.allocation higher; not enough assets to add to 1")
    }
    Amat <- cbind(Amat, -diag(n),diag(n))
    bvec <- c(bvec, rep(-max.allocation, n),rep(min.allocation,n))
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
    
    dvec <- exp_ret * i # This moves the solution along the EF
    
    sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
    
    eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution*colSums((covariance*sol$solution))))
    eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% exp_ret)
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
  
  eff$Std.Dev=eff$Std.Dev*sqrt(freq)
  eff$Exp.Return=eff$Exp.Return
  eff$sharpe=eff$Exp.Return/eff$Std.Dev
  eff$VaR20Days=eff$Std.Dev*1.96/sqrt(252)*sqrt(20)
  
  
  return(eff)
}

# Function to rank your Ptf
perc.rank <- function(x) trunc(rank(x))/length(x)

#function to create matrix only for relative view
P_MAtrix = function(refdate,AssetList,Direction){
  
  
  
  
}


#### Efficient Frontier function ####
eff.frontier_weight <- function (returns,exp_ret, short="no",selection,
                          max.allocation=NULL,
                          min.allocation=NULL,
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

    returns = returns[,selection]
    exp_ret = t(t(exp_ret[selection,]))
  
  
  if (is.positive.definite(cov(returns))){
    covariance <- cov(returns)
  }else{
    CovM=nearPD(cov(returns))
    covariance =CovM$mat
  }
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
    if(max(max.allocation) > 1 | min(max.allocation) <0){
      stop("max.allocation must be greater than 0 and less than 1")
    }
    if(sum(max.allocation) < 1){
      stop("Need to set max.allocation higher; not enough assets to add to 1")
    }
    Amat <- cbind(Amat, -diag(n),diag(n))
    bvec <- c(bvec, -max.allocation, min.allocation)
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
    
    dvec <- exp_ret * i # This moves the solution along the EF
    
    sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
    
    eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution*colSums((covariance*sol$solution))))
    eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% exp_ret)
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
  
  eff$Std.Dev=eff$Std.Dev*sqrt(freq)
  eff$Exp.Return=eff$Exp.Return
  eff$sharpe=eff$Exp.Return/eff$Std.Dev
  eff$VaR20Days=eff$Std.Dev*1.96/sqrt(252)*sqrt(20)
  

  
  return(eff)
  
}

# Function to rank your Ptf
perc.rank <- function(x) trunc(rank(x))/length(x)

#function to create matrix only for relative view
P_MAtrix = function(refdate,AssetList,Direction){
  
  
  
  
}




# Function to get absolute risk contribution
RiskContribution = function(weights,VarCov,Portfoliotype) {

    ptfVol = as.numeric(sqrt(t(weights) %*% VarCov %*% weights))
    MarginalcontrRisk = (VarCov %*% weights) / ptfVol

    AssetContribution = MarginalcontrRisk * weights * sqrt(252)
    AssetContributionPct = MarginalcontrRisk * weights / ptfVol * 100

    #names(AssetContribution) = colnames(weights)
    #names(AssetContributionPct) = colnames(weights)

    Output = data.frame(AbsolutContr = AssetContribution, 
        PctContr = AssetContributionPct,
        MrgContr = MarginalcontrRisk,
        Wgt = weights,
        Assets = rownames(weights),
        Type = rep(Portfoliotype, nrow(weights)))
    
    return(Output)

}


drawdown <- function(pnl) {
  pnl=ifelse(is.na(pnl),0,pnl)
  cum.pnl  <- c(1, cumprod(1+pnl))
  drawdown <- (cum.pnl / cummax(cum.pnl))-1
  return(tail(drawdown, -1))
}

maxdrawdown <- function(pnl)min(drawdown(pnl))


#### SetUp equal risk contribution portfolio ####

# objective function
eval_f <- function(w,cov.mat,vol.target) {
  
  vol <- sqrt(as.numeric(t(w) %*% cov.mat %*% w))
  marginal.contribution <- cov.mat %*% w / vol
  return( sum((vol/length(w) - w * marginal.contribution)^2) )
  
}


# numerical gradient approximation for solver
eval_grad_f <- function(w,cov.mat,vol.target) {
  
  out <- w
  
  for (i in 0:length(w)) {
    up <- dn <- w
    up[i] <- up[i]+.0001
    dn[i] <- dn[i]-.0001
    out[i] = (eval_f(up,cov.mat=cov.mat,vol.target=vol.target) - eval_f(dn,cov.mat=cov.mat,vol.target=vol.target))/.0002
  }
  
  return(out)
}





##### Function for portfolio optimization #### 
nextweekday <- function(date, wday) {
  date <- as.Date(date)
  diff <- wday - wday(date)
  if( diff < 0 )
    diff <- diff + 7
  return(date + diff)
}


TargetFun = function(w,exret=NA,sim=NA,alpha=.05,TRet=NA,type = "ES",period=NA){
  
  if(type=="ES"){
    ret = sort(sim %*% w)
    ret_w=sim %*% w
    n = length(ret)
    i = alpha * n
    
    if (!is.na(period)){
      
      xroll=rollapply(data = (1+ret_w), width = period, by=1, FUN = prod,fill=NA,align ="right",na.rm=F)-1
      xroll_sort=sort(xroll[!is.na(xroll)])
      
      g=length(xroll_sort)
      j=round(alpha * g,0)
      
      es = median(xroll_sort[1:j])
      
    }else{
      
      
      es = mean(ret[1:i])
      
      
      }
    
    
    return(-es)
    
  }else if (type=="St.Dev"){
    
    w = (as.matrix(w))
    varcov=cov(sim)
    ret = t(w) %*% varcov %*% w
    
    return(ret)
    
  }else if (type=="KurtSkew"){
    
    ret = as.numeric(sim %*% w)
    skew = -skewness(ret,na.rm = T)     
    kurt = kurtosis(ret,na.rm = T)
    ratio = kurt/skew
    
    return(ratio)
    
    
  }else if(type=="Kurtosis"){
    
    ret = as.numeric(sim %*% w)
    kurt = kurtosis(ret,na.rm = T)     
    
    return(kurt)
    
    
  }else if(type=="DDown"){
    
    ret = as.numeric(sim %*% w)
    cum.pnl  <- c(1, cumprod(1+ret/100))
    drawdown <- cum.pnl - cummax(cum.pnl)
    return(-min(tail(drawdown, -1))*100)
    
  }else if(type=="DownSideRisk"){
    
    ret = as.numeric(sim %*% w)
    ddrisk=DownsideDeviation(ret, MAR = 0)
    return(ddrisk)
    
    
    
  }else if(type=="TrackingError"){
    
    ret = as.numeric(sim %*% w)
    tracking=sd(ret)
    return(tracking)
    
  
  }else{
    
  }
  
  
}


#linear equality constraint
#note: nloptr requires all functions to have the same signature

eval_g0 <- function(w,exret=NA,sim=NA,alpha=NA,TRet=NA,type = "ES",period=NA) {
  return( sum(w) - 1 )
}

eval_g1 <- function(w, exret =NA, sim=NA,alpha=NA,TRet=NA,type = "ES",period=NA) {
  return( w %*% exret-TRet)
}




#numerical approximation of the gradient
des = function(w, exret =NA, sim=NA,alpha=NA,TRet=NA,type = "ES",period=NA){
  n = length(w)
  out = w;
  for (i in 0:n){
    up = w;
    dn = w;
    up[i] = up[i]+.0001
    dn[i] = dn[i]-.0001
    
    out[i] = (TargetFun(up,sim=sim,alpha=alpha,type = type,period = period ) - TargetFun(dn,sim=sim,alpha=alpha,type = type,period = period ))/.0002
    
    
  }
  return(out)
}

#use nloptr to check out gradient
#check.derivatives(w,TargetFun,des,sim=sim, alpha=.05,type="St.Dev")

#function to optimize -- a list of objective and gradient
toOpt = function(w, exret=NA, sim=NA,alpha=NA,TRet=NA,period=NA,type="ES"){
  list(objective=TargetFun(w,sim=sim,alpha=alpha,type=type,period = period),
       gradient=des(w,sim=sim,alpha=alpha,type=type))    
}


#equality constraint function. The jacobian is 1 for all variables
eqCon = function(w, exret =NA, sim=NA,alpha=NA,TRet=NA,period=NA,type="ES"){
  n=length(w)
  list(constraints=c(eval_g0(w,alpha=0.05,type=type),
                     eval_g1(w, exret =exret,TRet=TRet,type=type))
       ,
       jacobian=rbind(nl.jacobian(w,eval_g0,type=type),
                      nl.jacobian(w,TRet=TRet,exret = exret,eval_g1,type=type))
  )     
}



# Simple minimun val
eqConMin = function(w, exret =NA, sim=NA,alpha=NA,TRet=NA,period=NA,type="ES"){
  n=length(w)
  list(constraints=c(eval_g0(w,alpha=0.05,type=type))
       ,
       jacobian=rbind(nl.jacobian(w,eval_g0,type=type))
  )     
}


