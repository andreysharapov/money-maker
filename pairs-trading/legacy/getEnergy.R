start="2010-09-27"
sec="E"
stock_list<-c("XOM","PTR","CVX","PBR","BP","EC","TOT","SLB","CEO","STO","E","SNP","COP","OXY","SU","IMO","APC",
              "CNQ","NOV","APA","HAL","EOG","PSX","CVE","DVN","WMB","MRO","BHI","SDRL","MPC","HES","VLO","RIG",
              "NBL","ECA","PAA","TLM","CAM","CLR","NXY","PXD","CHK","ESV","SWN","FTI","RRC","MUR","CXO","WFT",
              "MMP","COG","DO","NE","CIE","EQT","EEP","HFC","LINE","KMR","YZC","PWE","CNX","IHS","DNR","BTU","OII",
              "CLB","TSO","BTE","WLL","QEP","YPF","ACMP","XEC","HP","SUN","CGV","PXP","SXL","BPL","KOS","OIS",
              "CZZ","NFX","RDC","APU","RGP","NBR","NGLS","EGN","NS","IOC","STR","SD","PGH","LNG","UPL","SM","SPN",
              "WPX","CVI","ERF","PVR","AHGP","ATW","DRQ","MDR","EXXI","LPI","GEL","RES","OAS","EVEP","INT","NRGY",
              "ROSE","PTEN","TDW","KOG","WNR","NTI","ARLP","NRP","PDS","ORIG","BRY","PACD","FET","WLT","UNT","BPT",
              "HLX","HEP","MMR","LUFK","BRS","APL","CLMT","ATLS","XCO","GPOR","SEMG","FGP","HK","VNR","CRR","ANR",
              "OILT","DK","SPH","WTI","LGCY","GTE","NSH","ACI","BBEP","EROC","EXH","EEQ","SGY","BBG","CLNE","CLD",
              "KEG","NOG","IO","CJES","CRZO","FST","XTEX","BCEI","PSE","EXLP","PDCE","SFY","PZE","CRK","QRE",
              "MMLP","HERO","EPL","TGA","MCF","ALJ","MHR","VQ","SZYM","PBT","SDT","REXX","XTXI","SN","NR","CWEI",
              "HEK","FTK","TLP","MPO","SUSP","PES","PKD","EGY","TTI","BAS","GDP","RNO","END","LRE","MEMP",
              "TESO","PQ","FXEN","HNR","BPZ","VOC","ZAZA","TPLM","ANW","PVA","MTRX","WG","CPE","MILL")
spx<-as.data.frame(coredata(dailyReturn(getSymbols("^GSPC",auto.assign=FALSE,src="yahoo",verbose=FALSE,from=start))))

stock_pickedE=c(stock_list[1])
quote<-getSymbols(stock_list[1],auto.assign=FALSE,src="yahoo",verbose=FALSE,from=start)
qret<-as.data.frame(coredata(dailyReturn(quote)))
betaE<-c((cov(spx,qret)/var(spx))[1,1])
dataE<-as.data.frame(coredata(Cl(quote)))

dl<-dim(dataE)[1]
for(j in 2:length(stock_list))
{
  quote<-getSymbols(stock_list[j],auto.assign=FALSE,src="yahoo",verbose=FALSE,from=start)
  x<-as.data.frame(coredata(Cl(quote)))
  if(dim(x)[1]==dl)
  {
    qret<-as.data.frame(coredata(dailyReturn(quote)))
    beta<-(cov(spx,qret)/var(spx))[1,1]
    dataE<-cbind(dataE,x)
    betaE<-c(betaE,beta)
    stock_pickedE<-c(stock_pickedE,stock_list[j])
  }
}

numpairs=0
first=c()
second=c()
potret=c()
johcoeff=c()
sink("D:\\LiC\\stocks\\E.txt")
for(i in 1:length(stock_pickedE))
{
  for(j in 1:length(stock_pickedE))
  {
    if(j>i)
    {
      johtest<-ca.jo(as.data.frame(t(rbind(t(dataE[1:dl,i]),t(dataE[1:dl,j])))),type="eigen",ecdet="none",K=2,spec="longrun")
      if(johtest@teststat[1]<johtest@cval[1,3]&&johtest@teststat[2]>johtest@cval[2,3]&&johtest@V[2,1]<0.0&&johtest@V[2,1]>-5.0)
      {
        v<-sqrt(var(dataE[range:dl,i]+johtest@V[2,1]*dataE[range:dl,j]))
        m<-mean(dataE[range:dl,i]+johtest@V[2,1]*dataE[range:dl,j])
        bp<-((betaE[i]*dataE[dl,i]+betaE[j]*johtest@V[2,1]*dataE[dl,j])/(dataE[dl,i]-johtest@V[2,1]*dataE[dl,j]))
        if(dataE[dl,i]+johtest@V[2,1]*dataE[dl,j]<m-2*v)
        {
          first<-c(first,stock_pickedE[i])
          second<-c(second,stock_pickedE[j])
          potret<-c(potret,2*v/dataE[dl,i])
          johcoeff<-c(johcoeff,johtest@V[2,1])
          numpairs=numpairs+1
        }
        if(dataE[dl,i]+johtest@V[2,1]*dataE[dl,j]>m+2*v)
        {
          first<-c(first,stock_pickedE[i])
          second<-c(second,stock_pickedE[j])
          potret<-c(potret,2*v/dataE[dl,j]/abs(johtest@V[2,1]))
          johcoeff<-c(johcoeff,johtest@V[2,1])
          numpairs=numpairs+1
        }        
      }
    }
  }
}

hl<-c()
cycle<-c()
meancycle<-c()
for(pair in 1:numpairs)
{
  i<-which(stock_pickedE==first[pair])
  j<-which(stock_pickedE==second[pair])
  pairdata<-dataE[1:dl,i]+johcoeff[pair]*dataE[1:dl,j]
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
  i<-which(stock_pickedE==firstsorted[k])
  j<-which(stock_pickedE==secondsorted[k])
  v<-sqrt(var(dataE[range:dl,i]+johcoeffsorted[k]*dataE[range:dl,j]))
  v1<-sqrt(var(dataE[1:dl,i]+johcoeffsorted[k]*dataE[1:dl,j]))
  m<-mean(dataE[range:dl,i]+johcoeffsorted[k]*dataE[range:dl,j])
  m1<-mean(dataE[1:dl,i]+johcoeffsorted[k]*dataE[1:dl,j])
  print("Half life")
  print(hlsorted[k])
  print("Approx. mean-reversion time")
  print(meancyclesorted[k])
  print("Johansen coefficient")
  print(johcoeffsorted[k])
  print("Correlation")
  print(cor(dataE[1:dl,i],dataE[1:dl,j]))
  print("Possible return")
  print(potretsorted[k])
  print("****************************")
  plot(dataE[1:dl,i]+johcoeffsorted[k]*dataE[1:dl,j],type="l",main=paste(firstsorted[k],secondsorted[k]),xlab="",ylab="",col="blue")
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
