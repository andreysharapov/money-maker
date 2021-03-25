beta<-function(s)
{
  require(quantmod)
  require(PerformanceAnalytics)
  getSymbols(s)
  start <- "2008-01-01"
  end<- "2010-01-01"
  range <- paste(start,"::",end,sep ="")
  pM <- cbind(get(s[1])[,6][range], get(s[2])[,6][range])
  rM<- Return.calculate(pM, method="simple")
  rM<- as.xts(rM)
  Mr<-cbind(get(s[15])[,6]["2008::2010"])
  Er<-Return.calculate(Mr, method="simple")
  Er<-as.xts(Er)
  Pr<-Return.portfolio(rM, weights = NULL, wealth.index = FALSE, contribution = FALSE, geometric = FALSE)
  Pr<-as.xts(Pr)
  betaS<-BetaCoVariance(rM, Er)
  betaP<-BetaCoVariance(Pr,Er)
  m<-list(c(betaS),c(betaP))
  return(m)
}