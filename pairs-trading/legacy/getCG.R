start="2010-09-21"
sec="CG"
stock_list<-c("UTX","MMM","CAT","BA","DE","LMT","TS","GD","RTN","CMI","NOC","CRH","IR","FAST","KUB","CNH","FLR","CX","COL","TDG","LLL","FLS",
              "PLL","DHI","LEN","PHM","VMC","TOL","ST","JEC","DCI","MAS","PWR","ERJ","XYL","MSM","KBR","PNR","AGCO","FBHS","BEAV","DRC","MLM",
              "NDSN","TKR","JHX","CBI","SPR","VMI","LECO","EIX","SPW","CFX","KMT","GGG","FLIR","GDI","RBC","TGI","URS","SHAW","WSO","TEX","AWI",
              "TRN","RBN","USG","VC","WWD","FWLT","TDY","ACM","LII","TTC","MIDD","GTLS","EXP","EME","ZBRA","LPX","SHS","ITT","XLS","HEI","MTW",
              "AIT","ESL","THO","HSC","MDC","GRP","ATK","STN","MTZ","ESLT","BECN","SPF","MTH","B","CUB","RYL","CPAC","ENTG","WAIR","TXI","GVA",
              "ICA","ACO","KBH","GFA","BGG","NC","LNN","ORB","TNC","SODA","SLCA","HXM","ASTE","BLT","AIR","NX","DXPE","DW","PRIM","RSTI","GY",
              "CASC","GRC","FOR","HCLP","SXI","AMRC","TPC","NPK","AVAV","AIMC","DY","CYD","HOV","APOG","JBT","AAON","HW","GLDD","CSTE","MYRG",
              "FIX","TITN","BZH","GIFI","ROCK","ALG","LAYN","MHO","ATRO","WGO","CVCO","GLPW","PIKE","CMCO","AMWD","ANEN","KTOS","KAI",
              "NWPX","LMIA","LDL","BKR","NCS","TWIN","AGX","ORN","XIN","IIIN","GHM","PGTI","STRL","DCO","HURC","PATK","CECE","BXC","TGE",
              "MPR","ERII","FTEK","HDNG","CVU","MNTX","SCX","HDSN","EDAC")
spx<-as.data.frame(coredata(dailyReturn(getSymbols("^GSPC",auto.assign=FALSE,src="yahoo",verbose=FALSE,from=start))))

stock_pickedCG=c(stock_list[1])
quote<-getSymbols(stock_list[1],auto.assign=FALSE,src="yahoo",verbose=FALSE,from=start)
qret<-as.data.frame(coredata(dailyReturn(quote)))
betaCG<-c((cov(spx,qret)/var(spx))[1,1])
dataCG<-as.data.frame(coredata(Cl(quote)))

dl<-dim(dataCG)[1]
for(j in 2:length(stock_list))
{
  quote<-getSymbols(stock_list[j],auto.assign=FALSE,src="yahoo",verbose=FALSE,from=start)
  x<-as.data.frame(coredata(Cl(quote)))
  if(dim(x)[1]==dl)
  {
    qret<-as.data.frame(coredata(dailyReturn(quote)))
    beta<-(cov(spx,qret)/var(spx))[1,1]
    dataCG<-cbind(dataCG,x)
    betaCG<-c(betaCG,beta)
    stock_pickedCG<-c(stock_pickedCG,stock_list[j])
  }
}

numpairs=0
first=c()
second=c()
potret=c()
johcoeff=c()
sink("D:\\LiC\\stocks\\CG.txt")
for(i in 1:length(stock_pickedCG))
{
  for(j in 1:length(stock_pickedCG))
  {
    if(j>i)
    {
      johtest<-ca.jo(as.data.frame(t(rbind(t(dataCG[1:dl,i]),t(dataCG[1:dl,j])))),type="eigen",ecdet="none",K=2,spec="longrun")
      if(johtest@teststat[1]<johtest@cval[1,3]&&johtest@teststat[2]>johtest@cval[2,3]&&johtest@V[2,1]<0.0&&johtest@V[2,1]>-5.0)
      {
        v<-sqrt(var(dataCG[range:dl,i]+johtest@V[2,1]*dataCG[range:dl,j]))
        m<-mean(dataCG[range:dl,i]+johtest@V[2,1]*dataCG[range:dl,j])
        bp<-((betaCG[i]*dataCG[dl,i]+betaCG[j]*johtest@V[2,1]*dataCG[dl,j])/(dataCG[dl,i]-johtest@V[2,1]*dataCG[dl,j]))
        if(dataCG[dl,i]+johtest@V[2,1]*dataCG[dl,j]<m-2*v)
        {
          first<-c(first,stock_pickedCG[i])
          second<-c(second,stock_pickedCG[j])
          potret<-c(potret,2*v/dataCG[dl,i])
          johcoeff<-c(johcoeff,johtest@V[2,1])
          numpairs=numpairs+1
        }
        if(dataCG[dl,i]+johtest@V[2,1]*dataCG[dl,j]>m+2*v)
        {
          first<-c(first,stock_pickedCG[i])
          second<-c(second,stock_pickedCG[j])
          potret<-c(potret,2*v/dataCG[dl,j]/abs(johtest@V[2,1]))
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
  i<-which(stock_pickedCG==first[pair])
  j<-which(stock_pickedCG==second[pair])
  pairdata<-dataCG[1:dl,i]+johcoeff[pair]*dataCG[1:dl,j]
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
  i<-which(stock_pickedCG==firstsorted[k])
  j<-which(stock_pickedCG==secondsorted[k])
  v<-sqrt(var(dataCG[range:dl,i]+johcoeffsorted[k]*dataCG[range:dl,j]))
  v1<-sqrt(var(dataCG[1:dl,i]+johcoeffsorted[k]*dataCG[1:dl,j]))
  m<-mean(dataCG[range:dl,i]+johcoeffsorted[k]*dataCG[range:dl,j])
  m1<-mean(dataCG[1:dl,i]+johcoeffsorted[k]*dataCG[1:dl,j])
  print("Half life")
  print(hlsorted[k])
  print("Approx. mean-reversion time")
  print(meancyclesorted[k])
  print("Johansen coefficient")
  print(johcoeffsorted[k])
  print("Correlation")
  print(cor(dataCG[1:dl,i],dataCG[1:dl,j]))
  print("Possible return")
  print(potretsorted[k])
  print("****************************")
  plot(dataCG[1:dl,i]+johcoeffsorted[k]*dataCG[1:dl,j],type="l",main=paste(firstsorted[k],secondsorted[k]),xlab="",ylab="",col="blue")
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
