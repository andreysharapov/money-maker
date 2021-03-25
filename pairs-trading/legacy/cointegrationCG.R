drv = dbDriver("MySQL")
con = dbConnect(drv,dbname="stockdata",user="root",pass="password")
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
stock_picked=c(stock_list[1])
dataCG<-t(dbGetQuery(con, paste("select open from price where ticker=\'",stock_list[1],"\'",sep="")))
dl<-dim(dataCG)[2]
for(i in 2:length(stock_list))
{
  x<-t(dbGetQuery(con, paste("select open from price where ticker=\'",stock_list[i],"\'",sep="")))
  if(dim(x)[2]==dl)
  {
    dataCG<-rbind(dataCG,x)
    stock_picked<-c(stock_picked,stock_list[i])
  }
  
}
dataCG<-t(dataCG)
dbDisconnect(con)

numpairs=0
sink("/home/quant/stocks/testresults")
for(i in 1:length(stock_picked))
{
  for(j in 1:length(stock_picked))
  {
    if(j>i)
    {
      johtest<-ca.jo(as.data.frame(t(rbind(t(dataCG[1:dl,i]),t(dataCG[1:dl,j])))),type="eigen",ecdet="none",K=2,spec="longrun")
      if(johtest@teststat[1]<johtest@cval[1,2]&&johtest@teststat[2]>johtest@cval[2,2]&&cor(dataCG[1:dl,i],dataCG[1:dl,j])>0.9)
      {
        v<-sqrt(var(dataCG[1:dl,i]+johtest@V[2,1]*dataCG[1:dl,j]))
        m<-mean(dataCG[1:dl,i]+johtest@V[2,1]*dataCG[1:dl,j])
        if((dataCG[dl,i]+johtest@V[2,1]*dataCG[dl,j]>m+2*v)||(dataCG[dl,i]+johtest@V[2,1]*dataCG[dl,j]<m-2*v))
        {
          print(stock_picked[i])
          print(i)
          print(stock_picked[j])
          print(j)
          print(johtest@V[1:2,1])
          print(cor(dataCG[1:dl,i],dataCG[1:dl,j]))
          print("****************************")
          plot(dataCG[1:dl,i]+johtest@V[2,1]*dataCG[1:dl,j],type="l",main=paste(stock_picked[i],stock_picked[j]))
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
      johtest<-ca.jo(as.data.frame(t(rbind(t(dataCG[1:dl,i]),t(dataCG[1:dl,j])))),type="eigen",ecdet="none",K=2,spec="longrun")
      if(johtest@teststat[1]<johtest@cval[1,2]&&johtest@teststat[2]>johtest@cval[2,2]&&cor(dataCG[1:dl,i],dataCG[1:dl,j])>0.9)
      {
        v<-sqrt(var(dataCG[1:dl,i]+johtest@V[2,1]*dataCG[1:dl,j]))
        m<-mean(dataCG[1:dl,i]+johtest@V[2,1]*dataCG[1:dl,j])
        if((dataCG[dl,i]+johtest@V[2,1]*dataCG[dl,j]>m+v)||(dataCG[dl,i]+johtest@V[2,1]*dataCG[dl,j]<m-v))
        {
          print(stock_picked[i])
          print(i)
          print(stock_picked[j])
          print(j)
          print(johtest@V[1:2,1])
          print(cor(dataCG[1:dl,i],dataCG[1:dl,j]))
          print("****************************")
          plot(dataCG[1:dl,i]+johtest@V[2,1]*dataCG[1:dl,j],type="l",main=paste(stock_picked[i],stock_picked[j]))
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