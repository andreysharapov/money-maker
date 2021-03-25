start="2010-09-21"
sec="BM"
stock_list<-c("BHP","VALE","RIO","BBL","MON","DD","ABX","FCX","DOW","POT","GG","SYT","PX","SSL","SCCO","LYB","NEM","MOS","PKX","MT","PCP",
              "TCK","APD","PPG","GGB","SQM","AGU","IP","SHW","WY","CF","AU","AUY","SLW","PH","NUE","DOV","EGO","GOLD","AA","BVN","GFI","SIAL",
              "CCJ","SID","AEM","FMC","EMN","TRQ","PCL","BLL","ARG","CE","RYN","BAK","SLT","CLF","CCK","LUK","ACH","IAG","NGD","RGLD","MWV",
              "VAL","ALB","IFF","RKT","WLK","GRA","RS","TNH","FBR","TX","HUN","ROC","ATI","HMY","ATR","NEU","CSL","ANV","OI","PKG","BMS",
              "X","CYT","SON","SEE","MTL","SLGN","PAAS","CRS","MEOH","UFS","SMG","STLD","CBT","CMP","AG","CR","GPK","TIE","CDE","AWC","GEF",
              "ATU","SMS","KRO","SHI","SXT","UAN","OLN","IPI","PDH","MLI","CHMT","PPO","HBM","RXN","CMC","FUL","AUQ","HL","WOR","CW","NG",
              "POL","SWC","RFP","GGC","RNF","MCP","SSD","WTS","MTX","BKI","SSRI","GSM","MUX","KALU","RAVN","HWD","BCPC","IPHS","ROLL","GORO",
              "RBY","SVM","SWM","KS","EXK","KAMN","BAA","CLW","LXU","AVD","NSU","TRS","MATW","CCC","BZ","WDFC","DEL","SCHN","NPO","UFPI",
              "SA","AZK","KRA","KIOR","GLT","SHLM","KOP","KDN","IOSP","RTI","AIN","MWA","CENX","SNHY","HAYN","AKS","TPCG","MVG","CIR","TGB",
              "OMG","EMO","GFF","WIRE","KWR","TC","NL","MUSA","TG","DNN","TREX","MYE","RTK","NEWP","NOR","TRX","NP","WPP","ZINC","FF","HWKN",
              "GSS","OMN","AZC","PZG","PAL","MERC","FSIN","FSTR","NAK","EDG","FOE","PLPC","CAS","GPL","KGN","LNDC","GMO","SVLC","VGZ","USAP",
              "ACET","THM","ZAGG","AUMN","AXU","ADES","ARSD","MCC","DRD","UEC","YONG","BOOM","KMG","GRZ","PLM","PLG","CXDC","REGI","AMRS",
              "AVL","HNH","AP","SHLO","ZEUS","MDW","CCIX","AAU","UAMY","SEH","GSE","GPRE","XRA","CIX")
spx<-as.data.frame(coredata(dailyReturn(getSymbols("^GSPC",auto.assign=FALSE,src="yahoo",verbose=FALSE,from=start))))

stock_pickedBM=c(stock_list[1])
quote<-getSymbols(stock_list[1],auto.assign=FALSE,src="yahoo",verbose=FALSE,from=start)
qret<-as.data.frame(coredata(dailyReturn(quote)))
betaBM<-c((cov(spx,qret)/var(spx))[1,1])
dataBM<-as.data.frame(coredata(Cl(quote)))

dl<-dim(dataBM)[1]
for(j in 2:length(stock_list))
{
  quote<-getSymbols(stock_list[j],auto.assign=FALSE,src="yahoo",verbose=FALSE,from=start)
  x<-as.data.frame(coredata(Cl(quote)))
  if(dim(x)[1]==dl)
  {
    qret<-as.data.frame(coredata(dailyReturn(quote)))
    beta<-(cov(spx,qret)/var(spx))[1,1]
    dataBM<-cbind(dataBM,x)
    betaBM<-c(betaBM,beta)
    stock_pickedBM<-c(stock_pickedBM,stock_list[j])
  }
}

numpairs=0
first=c()
second=c()
potret=c()
johcoeff=c()
sink("D:\\LiC\\stocks\\BM.txt")
for(i in 1:length(stock_pickedBM))
{
  for(j in 1:length(stock_pickedBM))
  {
    if(j>i)
    {
      johtest<-ca.jo(as.data.frame(t(rbind(t(dataBM[1:dl,i]),t(dataBM[1:dl,j])))),type="eigen",ecdet="none",K=2,spec="longrun")
      if(johtest@teststat[1]<johtest@cval[1,3]&&johtest@teststat[2]>johtest@cval[2,3]&&johtest@V[2,1]<0.0&&johtest@V[2,1]>-5.0)
      {
        v<-sqrt(var(dataBM[range:dl,i]+johtest@V[2,1]*dataBM[range:dl,j]))
        m<-mean(dataBM[range:dl,i]+johtest@V[2,1]*dataBM[range:dl,j])
        bp<-((betaBM[i]*dataBM[dl,i]+betaBM[j]*johtest@V[2,1]*dataBM[dl,j])/(dataBM[dl,i]-johtest@V[2,1]*dataBM[dl,j]))
        if(dataBM[dl,i]+johtest@V[2,1]*dataBM[dl,j]<m-2*v)
        {
          first<-c(first,stock_pickedBM[i])
          second<-c(second,stock_pickedBM[j])
          potret<-c(potret,2*v/dataBM[dl,i])
          johcoeff<-c(johcoeff,johtest@V[2,1])
          numpairs=numpairs+1
        }
        if(dataBM[dl,i]+johtest@V[2,1]*dataBM[dl,j]>m+2*v)
        {
          first<-c(first,stock_pickedBM[i])
          second<-c(second,stock_pickedBM[j])
          potret<-c(potret,2*v/dataBM[dl,j]/abs(johtest@V[2,1]))
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
  i<-which(stock_pickedBM==first[pair])
  j<-which(stock_pickedBM==second[pair])
  pairdata<-dataBM[1:dl,i]+johcoeff[pair]*dataBM[1:dl,j]
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

for(k in 1:30)
{
  print(firstsorted[k])
  print(secondsorted[k])
  i<-which(stock_pickedBM==firstsorted[k])
  j<-which(stock_pickedBM==secondsorted[k])
  v<-sqrt(var(dataBM[range:dl,i]+johcoeffsorted[k]*dataBM[range:dl,j]))
  v1<-sqrt(var(dataBM[1:dl,i]+johcoeffsorted[k]*dataBM[1:dl,j]))
  m<-mean(dataBM[range:dl,i]+johcoeffsorted[k]*dataBM[range:dl,j])
  m1<-mean(dataBM[1:dl,i]+johcoeffsorted[k]*dataBM[1:dl,j])
  print("Half life")
  print(hlsorted[k])
  print("Approx. mean-reversion time")
  print(meancyclesorted[k])
  print("Johansen coefficient")
  print(johcoeffsorted[k])
  print("Correlation")
  print(cor(dataBM[1:dl,i],dataBM[1:dl,j]))
  print("Possible return")
  print(potretsorted[k])
  print("****************************")
  plot(dataBM[1:dl,i]+johcoeffsorted[k]*dataBM[1:dl,j],type="l",main=paste(firstsorted[k],secondsorted[k]),xlab="",ylab="")
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
