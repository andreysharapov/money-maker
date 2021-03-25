start="2011-09-25"
sec="F"
stock_list<-c("GE","WFC","HBC","JPM","C","BAC","RY","LFC","TD","WBK","ITUB","SAN","AXP","BBD","USB","BNS",
              "UNH","GS","MA","SMFG","BBVA","LYG","BCS","BLK","BMO","MET","DB","PNC","PUK","COF","MS","ING","CM","AFG",
              "CS","BK","PRU","BEN","ACE","TRV","BBT","AFL","MFC","STT","CB","IBN","WLP","DFS","CME","MMC","ALL","SCHW",
              "BX","NLY","AON","AV","L","TROW","SHG","STI","FITB","CI","NMR","KB","AET","CIB","BCH","PGR","AMP","HUM",
              "MTB","NTRS","IVZ","RF","AEG","ICE","BAP","FIS","AMTD","HIG","KEY","CIT","SLM","CNA","XL","OAK","LNC",
              "WSH","CINF","AMG","CMA","CEF","HBAN","UNM","RE","CVH","ACGL","WRB","RJF","TMK","FNF","MSCI","RGA",
              "AJG","FRC","PBCT","NDAQ","BOKF","HCBK","RNR","ARCC","SEIC","CBOE","ZION","ACAS","LAZ","LM","OZM","CFR","OCN",
              "KKR","HCC","GPN","EV","LPLA","VR","JEF","EWBC","SBNY","BR","AIZ","AGO","FNFG","CYN","GNW","AWH","PRA","WDR",
              "SIVB","HBHC","KYN","NSM","ETFC","PB","DNP","ORI","FHN","BKU","PL","CNO","ASBC","CACC","MBI","FII","BOH",
              "MCY","FIG","FAF","AHL","ACG","ASPS","VLY","FULT","FAX","UMBF","SUSQ","WBS","PSEC","HNT","TCB","FMER","CFFN","SF",
              "AFSI","APO","SNV","TCBI","PRI","BPOP","RATE","WAFD","JNS","CSE","THG","AINV","TRMK","FNB","CEM","SYA",
              "UMPQ","CNS","BXS","GHL","NPBC","PTP","SFG","CATY","WTFC")
spx<-as.data.frame(coredata(dailyReturn(getSymbols("^GSPC",auto.assign=FALSE,src="yahoo",verbose=FALSE,from=start))))

stock_pickedF=c(stock_list[1])
quote<-getSymbols(stock_list[1],auto.assign=FALSE,src="yahoo",verbose=FALSE,from=start)
qret<-as.data.frame(coredata(dailyReturn(quote)))
betaF<-c((cov(spx,qret)/var(spx))[1,1])
dataF<-as.data.frame(coredata(Cl(quote)))

dl<-dim(dataF)[1]
for(j in 2:length(stock_list))
{
  quote<-getSymbols(stock_list[j],auto.assign=FALSE,src="yahoo",verbose=FALSE,from=start)
  x<-as.data.frame(coredata(Cl(quote)))
  if(dim(x)[1]==dl)
  {
    qret<-as.data.frame(coredata(dailyReturn(quote)))
    beta<-(cov(spx,qret)/var(spx))[1,1]
    dataF<-cbind(dataF,x)
    betaF<-c(betaF,beta)
    stock_pickedF<-c(stock_pickedF,stock_list[j])
  }
}

numpairs=0
range=1
first=c()
second=c()
potret=c()
johcoeff=c()
sink("/Users/andreysharapov/stockdata/financials.txt")
for(i in 1:length(stock_pickedF))
{
  for(j in 1:length(stock_pickedF))
  {
    if(j>i)
    {
      dataC<-as.data.frame(t(rbind(t(dataF[1:dl,i]),t(dataF[1:dl,j]))))
      
      johtest<-ca.jo(dataC,type="eigen",ecdet="none",K=2,spec="longrun")
      
#       testCT<-TVECM.SeoTest(dataC,lag=2, beta=1, trim=0.1,nboot=10, plot=FALSE,check=TRUE)
#       if(testCT$PvalBoot<0.05) {
#           print(i)
#           print(j)
#           print('******************')
#       }
#       if(johtest@teststat[1]<johtest@cval[1,3]&&johtest@teststat[2]>johtest@cval[2,3]){
#         if((i==1&&j==2)||(i==1&&j==4)||(i==1&&j==6)||(i==1&&j==7)||(i==1&&j==8)) {
#           print('******************')
#         }
#       }
      
      if(johtest@teststat[1]<johtest@cval[1,3]&&johtest@teststat[2]>johtest@cval[2,3]&&johtest@V[2,1]<0.0&&johtest@V[2,1]>-5.0)
      {
                  print(i)
                  print(j)
                  print('******************')
        v<-sqrt(var(dataF[range:dl,i]+johtest@V[2,1]*dataF[range:dl,j]))
        m<-mean(dataF[range:dl,i]+johtest@V[2,1]*dataF[range:dl,j])
        bp<-((betaF[i]*dataF[dl,i]+betaF[j]*johtest@V[2,1]*dataF[dl,j])/(dataF[dl,i]-johtest@V[2,1]*dataF[dl,j]))
        if(dataF[dl,i]+johtest@V[2,1]*dataF[dl,j]<m-2*v)
        {
          first<-c(first,stock_pickedF[i])
          second<-c(second,stock_pickedF[j])
          potret<-c(potret,2*v/dataF[dl,i])
          johcoeff<-c(johcoeff,johtest@V[2,1])
          numpairs=numpairs+1
        }
        if(dataF[dl,i]+johtest@V[2,1]*dataF[dl,j]>m+2*v)
        {
          first<-c(first,stock_pickedF[i])
          second<-c(second,stock_pickedF[j])
          potret<-c(potret,2*v/dataF[dl,j]/abs(johtest@V[2,1]))
          johcoeff<-c(johcoeff,johtest@V[2,1])
          numpairs=numpairs+1
        }        
      }
    }
  }
}

cycle<-c()
meancycle<-c()
hl<-c()
for(pair in 1:numpairs)
{
  i<-which(stock_pickedF==first[pair])
  j<-which(stock_pickedF==second[pair])
  pairdata<-dataF[1:dl,i]+johcoeff[pair]*dataF[1:dl,j]
  hl<-c(hl,log(2)/fitOU(pairdata)[1])
  m<-mean(pairdata[range:dl])
  v<-sqrt(var(pairdata[range:dl]))
  endday=2
  for(k in endday:dl)
  {
    if((pairdata[k]>m-2*v)&&(pairdata[k-1]<m-2*v))
    {
      startday<-k
      for(kk in startday:dl)
      {
        if((pairdata[kk]>m)&&(pairdata[kk-1]<m))
        {
          endday<-kk
          cycle<-c(cycle,endday-startday)
          break
        }
      }
     }
       
    if((pairdata[k]<m+2*v)&&(pairdata[k-1]>m+2*v))
    {
         startday<-k
         for(kk in startday:dl)
         {
           if((pairdata[kk]<m)&&(pairdata[kk-1]>m))
           {
             endday<-kk
             cycle<-c(cycle,endday-startday)
             break
           }
         }
       }       
  }
  meancycle<-c(meancycle,mean(cycle))
  cycle<-c()
} 


hlsorted<-sort(hl)
hlord<-order(hl)
firstsorted<-first[hlord]
secondsorted<-second[hlord]
johcoeffsorted<-johcoeff[hlord]
meancyclesorted<-meancycle[hlord]
potretsorted<-potret[hlord]

for(k in 1:numpairs)
{
  print(firstsorted[k])
  print(secondsorted[k])
  i<-which(stock_pickedF==firstsorted[k])
  j<-which(stock_pickedF==secondsorted[k])
  v<-sqrt(var(dataF[range:dl,i]+johcoeffsorted[k]*dataF[range:dl,j]))
  v1<-sqrt(var(dataF[1:dl,i]+johcoeffsorted[k]*dataF[1:dl,j]))
  m<-mean(dataF[range:dl,i]+johcoeffsorted[k]*dataF[range:dl,j])
  m1<-mean(dataF[1:dl,i]+johcoeffsorted[k]*dataF[1:dl,j])
  print("Half life")
  print(hlsorted[k])
  print("Approx. mean-reversion time")
  print(meancyclesorted[k])
  print("Johansen coefficient")
  print(johcoeffsorted[k])
  print("Correlation")
  print(cor(dataF[1:dl,i],dataF[1:dl,j]))
  print("Possible return")
  print(potretsorted[k])
  print("****************************")
  plot(dataF[1:dl,i]+johcoeffsorted[k]*dataF[1:dl,j],type="l",main=paste(firstsorted[k],secondsorted[k]),xlab="",ylab="")
  abline(m,0,col="green")
  abline(m+v,0)
  abline(m-v,0)      
  abline(m+2*v,0,col="red")
  abline(m-2*v,0,col="red")
  abline(m1,0,col="blue",lwd=2)
  abline(m1+v1,0,lwd=2)
  abline(m1-v1,0,lwd=2)      
  abline(m1+2*v1,0,col="red",lwd=2)
  abline(m1-2*v1,0,col="red",lwd=2)  
}
sink()
print("The number of pairs is")
print(numpairs)

