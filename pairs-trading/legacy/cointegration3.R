drv = dbDriver("MySQL")
con = dbConnect(drv,dbname="stockdata",user="root",pass="password")
stock_list<-c("GE","WFC","HBC","JPM","C","BAC","RY","LFC","TD","WBK","ITUB","SAN","AXP","BBD","USB","BNS","AVF",
              "UNH","GS","MA","SMFG","BBVA","LYG","BCS","BLK","BMO","MET","DB","PNC","PUK","COF","MS","ING","CM","AFG",
              "CS","BK","PRU","BEN","ACE","TRV","BBT","AFL","MFC","STT","CB","IBN","WLP","DFS","CME","MMC","ALL","SCHW",
              "BX","NLY","AON","AV","L","TROW","SHG","STI","FITB","CI","NMR","KB","AET","CIB","BCH","PGR","AMP","HUM",
              "MTB","NTRS","IVZ","RF","AEG","ICE","BAP","FIS","AMTD","HIG","KEY","CIT","SLM","CNA","XL","OAK","LNC","NYX",
              "WSH","CINF","AMG","CMA","NYB","CEF","HBAN","UNM","RE","CVH","ACGL","WRB","RJF","TMK","FNF","MSCI","RGA",
              "AJG","FRC","PBCT","NDAQ","BOKF","HCBK","RNR","ARCC","SEIC","CBOE","ZION","ACAS","LAZ")
stock_picked=c(stock_list[1])
data<-t(dbGetQuery(con, paste("select open from price where ticker=\'",stock_list[1],"\'",sep="")))
for(i in 2:length(stock_list))
{
  x<-t(dbGetQuery(con, paste("select open from price where ticker=\'",stock_list[i],"\'",sep="")))
  if(dim(x)[2]==677)
  {
    data<-rbind(data,x)
    stock_picked<-c(stock_picked,stock_list[i])
  }
  
}
data<-t(data)
dbDisconnect(con)

for(i in 1:length(stock_picked))
{
  for(j in 1:length(stock_picked))
    {
    for(k in 1:length(stock_picked))
    {
    if(i!=j&&i!=k&&j!=k)
    {
      johtest<-ca.jo(as.data.frame(t(rbind(t(data[1:677,i]),t(data[1:677,j]),t(data[1:677,k])))),type="eigen",ecdet="none",K=2,spec="longrun")
      if(johtest@teststat[2]<johtest@cval[2,2]&&johtest@teststat[3]>johtest@cval[3,2])
      {
        print(stock_picked[i])
        print(i)
        print(stock_picked[j])
        print(j)
        print(stock_picked[k])
        print(k)       
        print(johtest@V[1:3,1])
        print("**********One relation **************")
        plot(data[1:677,i]+johtest@V[2,1]*data[1:677,j]+johtest@V[3,1]*data[1:677,k],type="l",main=paste(stock_picked[i],stock_picked[j],stock_picked[k]))
        abline(mean(data[1:677,i]+johtest@V[2,1]*data[1:677,j]+johtest@V[3,1]*data[1:677,k]),0)        
      }
      if(johtest@teststat[2]>johtest@cval[2,2]&&johtest@teststat[3]>johtest@cval[3,2]&&johtest@teststat[1]<johtest@cval[1,2])
      {
        print(stock_picked[i])
        print(i)
        print(stock_picked[j])
        print(j)
        print(stock_picked[k])
        print(k)        
        print(johtest@V[1:3,1])
        print("*********Two relations**************")
        plot(data[1:677,i]+johtest@V[2,1]*data[1:677,j]+johtest@V[3,1]*data[1:677,k],type="l",main=paste(stock_picked[i],stock_picked[j],stock_picked[k]))
        abline(mean(data[1:677,i]+johtest@V[2,1]*data[1:677,j]+johtest@V[3,1]*data[1:677,k]),0)
        plot(data[1:677,i]+johtest@V[2,2]*data[1:677,j]+johtest@V[3,2]*data[1:677,k],type="l",main=paste(stock_picked[i],stock_picked[j],stock_picked[k]))
        abline(mean(data[1:677,i]+johtest@V[2,2]*data[1:677,j]+johtest@V[3,2]*data[1:677,k]),0)        
      }      
     }
    }
  }
}


