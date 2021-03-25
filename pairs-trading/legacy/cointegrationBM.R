drv = dbDriver("MySQL")
con = dbConnect(drv,dbname="stockdata",user="root",pass="password")
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
stock_picked=c(stock_list[1])
dataBM<-t(dbGetQuery(con, paste("select open from price where ticker=\'",stock_list[1],"\'",sep="")))
dl<-dim(dataBM)[2]
for(i in 2:length(stock_list))
{
  x<-t(dbGetQuery(con, paste("select open from price where ticker=\'",stock_list[i],"\'",sep="")))
  if(dim(x)[2]==dl)
  {
    dataBM<-rbind(dataBM,x)
    stock_picked<-c(stock_picked,stock_list[i])
  }
  
}
dataBM<-t(dataBM)
dbDisconnect(con)

numpairs=0
sink("/home/quant/stocks/testresults")
for(i in 1:length(stock_picked))
{
  for(j in 1:length(stock_picked))
  {
    if(j>i)
    {
      johtest<-ca.jo(as.data.frame(t(rbind(t(dataBM[1:dl,i]),t(dataBM[1:dl,j])))),type="eigen",ecdet="none",K=2,spec="longrun")
      if(johtest@teststat[1]<johtest@cval[1,2]&&johtest@teststat[2]>johtest@cval[2,2]&&cor(dataBM[1:dl,i],dataBM[1:dl,j])>0.9)
      {
        v<-sqrt(var(dataBM[1:dl,i]+johtest@V[2,1]*dataBM[1:dl,j]))
        m<-mean(dataBM[1:dl,i]+johtest@V[2,1]*dataBM[1:dl,j])
        if((dataBM[dl,i]+johtest@V[2,1]*dataBM[dl,j]>m+2*v)||(dataBM[dl,i]+johtest@V[2,1]*dataBM[dl,j]<m-2*v))
        {
          print(stock_picked[i])
          print(i)
          print(stock_picked[j])
          print(j)
          print(johtest@V[1:2,1])
          print(cor(dataBM[1:dl,i],dataBM[1:dl,j]))
          print("****************************")
          plot(dataBM[1:dl,i]+johtest@V[2,1]*dataBM[1:dl,j],type="l",main=paste(stock_picked[i],stock_picked[j]))
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
      johtest<-ca.jo(as.data.frame(t(rbind(t(dataBM[1:dl,i]),t(dataBM[1:dl,j])))),type="eigen",ecdet="none",K=2,spec="longrun")
      if(johtest@teststat[1]<johtest@cval[1,2]&&johtest@teststat[2]>johtest@cval[2,2]&&cor(dataBM[1:dl,i],dataBM[1:dl,j])>0.9)
      {
        v<-sqrt(var(dataBM[1:dl,i]+johtest@V[2,1]*dataBM[1:dl,j]))
        m<-mean(dataBM[1:dl,i]+johtest@V[2,1]*dataBM[1:dl,j])
        if((dataBM[dl,i]+johtest@V[2,1]*dataBM[dl,j]>m+v)||(dataBM[dl,i]+johtest@V[2,1]*dataBM[dl,j]<m-v))
        {
          print(stock_picked[i])
          print(i)
          print(stock_picked[j])
          print(j)
          print(johtest@V[1:2,1])
          print(cor(dataBM[1:dl,i],dataBM[1:dl,j]))
          print("****************************")
          plot(dataBM[1:dl,i]+johtest@V[2,1]*dataBM[1:dl,j],type="l",main=paste(stock_picked[i],stock_picked[j]))
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