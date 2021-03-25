drv = dbDriver("MySQL")
con = dbConnect(drv,dbname="stockdata",user="root",pass="password")
stock_list<-c("GE","WFC","HBC","JPM","C","BAC","RY","LFC","TD","WBK","ITUB","SAN","AXP","BBD","USB","BNS","AVF",
              "UNH","GS","MA","SMFG","BBVA","LYG","BCS","BLK","BMO","MET","DB","PNC","PUK","COF","MS","ING","CM","AFG",
              "CS","BK","PRU","BEN","ACE","TRV","BBT","AFL","MFC","STT","CB","IBN","WLP","DFS","CME","MMC","ALL","SCHW",
              "BX","NLY","AON","AV","L","TROW","SHG","STI","FITB","CI","NMR","KB","AET","CIB","BCH","PGR","AMP","HUM",
              "MTB","NTRS","IVZ","RF","AEG","ICE","BAP","FIS","AMTD","HIG","KEY","CIT","SLM","CNA","XL","OAK","LNC","NYX",
              "WSH","CINF","AMG","CMA","NYB","CEF","HBAN","UNM","RE","CVH","ACGL","WRB","RJF","TMK","FNF","MSCI","RGA",
              "AJG","FRC","PBCT","NDAQ","BOKF","HCBK","RNR","ARCC","SEIC","CBOE","ZION","ACAS","LAZ","LM","OZM","CFR","OCN",
              "KKR","HCC","GPN","EV","LPLA","VR","JEF","EWBC","SBNY","BR","AIZ","AGO","FNFG","CYN","GNW","AWH","PRA","WDR",
              "SIVB","HBHC","KYN","NSM","ETFC","PB","DNP","ORI","FHN","BKU","ALTE","PL","CNO","ASBC","CACC","MBI","FII","BOH",
              "MCY","FIG","FAF","AHL","ACG","ASPS","VLY","FULT","FAX","UMBF","SUSQ","WBS","PSEC","HNT","TCB","FMER","CFFN","SF",
              "AFSI","APO","SNV","TCBI","PRI","KFN","BPOP","RATE","WAFD","JNS","CSE","THG","AINV","TRMK","FNB","CEM","SYA",
              "UMPQ","CNS","BXS","GHL","NPBC","PTP","SFG","CATY","WTFC")
stock_picked=c(stock_list[1])
dataF<-t(dbGetQuery(con, paste("select close from price where ticker=\'",stock_list[1],"\'",sep="")))
dl<-dim(dataF)[2]
for(i in 2:length(stock_list))
{
   x<-t(dbGetQuery(con, paste("select close from price where ticker=\'",stock_list[i],"\'",sep="")))
   if(dim(x)[2]==dl)
   {
     dataF<-rbind(dataF,x)
     stock_picked<-c(stock_picked,stock_list[i])
   }
   
}
dataF<-t(dataF)
dbDisconnect(con)

numpairs=0
sink("/home/quant/stocks/testresults")
for(i in 1:length(stock_picked))
{
  for(j in 1:length(stock_picked))
  {
    if(j>i)
    {
      johtest<-ca.jo(as.data.frame(t(rbind(t(dataF[1:dl,i]),t(dataF[1:dl,j])))),type="eigen",ecdet="none",K=2,spec="longrun")
      if(johtest@teststat[1]<johtest@cval[1,3]&&johtest@teststat[2]>johtest@cval[2,3])
      {
        v<-sqrt(var(dataF[dl-130:dl,i]+johtest@V[2,1]*dataF[dl-130:dl,j]))
        m<-mean(dataF[dl-130:dl,i]+johtest@V[2,1]*dataF[dl-130:dl,j])
        if((dataF[dl,i]+johtest@V[2,1]*dataF[dl,j]>m+2*v)||(dataF[dl,i]+johtest@V[2,1]*dataF[dl,j]<m-2*v))
        {
          print(stock_picked[i])
          print(i)
          print(stock_picked[j])
          print(j)
          print(johtest@V[1:2,1])
          print(cor(dataF[1:dl,i],dataF[1:dl,j]))
          print("****************************")
          plot(dataF[1:dl,i]+johtest@V[2,1]*dataF[1:dl,j],type="l",main=paste(stock_picked[i],stock_picked[j]))
          abline(m,0,col="green")
          abline(m+v,0)
          abline(m-v,0)      
          abline(m+2*v,0,col="red")
          abline(m-2*v,0,col="red") 
          numpairs=numpairs+1
        }
      }
    }
  }
}
sink()
print("The number of pairs is")
print(numpairs)

numpairs=0
sink("/home/quant/stocks/testresults")
for(i in 1:length(stock_picked))
{
  for(j in 1:length(stock_picked))
  {
    if(j>i)
    {
      johtest<-ca.jo(as.data.frame(t(rbind(t(dataF[1:dl,i]),t(dataF[1:dl,j])))),type="eigen",ecdet="const",K=2,spec="longrun")
      if(johtest@teststat[1]<johtest@cval[1,3]&&johtest@teststat[2]>johtest@cval[2,3])
      {
        v<-sqrt(var(dataF[dl-130:dl,i]+johtest@V[2,1]*dataF[dl-130:dl,j]))
        m<-mean(dataF[dl-130:dl,i]+johtest@V[2,1]*dataF[dl-130:dl,j])
        if((dataF[dl,i]+johtest@V[2,1]*dataF[dl,j]>m+2*v)||(dataF[dl,i]+johtest@V[2,1]*dataF[dl,j]<m-2*v))
        {
          print(stock_picked[i])
          print(i)
          print(stock_picked[j])
          print(j)
          print(johtest@V[1:2,1])
          print(cor(dataF[1:dl,i],dataF[1:dl,j]))
          print("****************************")
          plot(dataF[1:dl,i]+johtest@V[2,1]*dataF[1:dl,j],type="l",main=paste(stock_picked[i],stock_picked[j]))
          abline(m,0,col="green")
          abline(m+v,0)
          abline(m-v,0)      
          abline(m+2*v,0,col="red")
          abline(m-2*v,0,col="red") 
          numpairs=numpairs+1
        }
      }
    }
  }
}
sink()
print("The number of pairs is")
print(numpairs)

numpairs=0
sink("/home/quant/stocks/testresults")
for(i in 1:length(stock_picked))
{
  for(j in 1:length(stock_picked))
  {
    if(j>i)
    {
      johtest<-ca.jo(as.data.frame(t(rbind(t(dataF[1:dl,i]),t(dataF[1:dl,j])))),type="eigen",ecdet="trend",K=2,spec="longrun")
      if(johtest@teststat[1]<johtest@cval[1,3]&&johtest@teststat[2]>johtest@cval[2,3])
      {
        v<-sqrt(var(dataF[dl-130:dl,i]+johtest@V[2,1]*dataF[dl-130:dl,j]))
        m<-mean(dataF[dl-130:dl,i]+johtest@V[2,1]*dataF[dl-130:dl,j])
        if((dataF[dl,i]+johtest@V[2,1]*dataF[dl,j]>m+2*v)||(dataF[dl,i]+johtest@V[2,1]*dataF[dl,j]<m-2*v))
        {
          print(stock_picked[i])
          print(i)
          print(stock_picked[j])
          print(j)
          print(johtest@V[1:2,1])
          print(cor(dataF[1:dl,i],dataF[1:dl,j]))
          print("****************************")
          plot(dataF[1:dl,i]+johtest@V[2,1]*dataF[1:dl,j],type="l",main=paste(stock_picked[i],stock_picked[j]))
          abline(m,0,col="green")
          abline(m+v,0)
          abline(m-v,0)      
          abline(m+2*v,0,col="red")
          abline(m-2*v,0,col="red") 
          numpairs=numpairs+1
        }
      }
    }
  }
}
sink()
print("The number of pairs is")
print(numpairs)

