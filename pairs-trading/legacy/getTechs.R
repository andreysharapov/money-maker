start="2010-09-21"
sec="T"
stock_list<-c("AAPL","MSFT","IBM","GOOG","ORCL","INTC","QCOM","CSCO","SAP","TSM","EMC","HON","ABB","VMW","CAJ","BIDU","DHR",
              "EMR","HPQ","TXN","ERIC","INFY","ASML","CRM","WIT","CTSH","BRCM","GLW","DELL","INTU","ETN","ADBE","TEL","KYO",
              "CTXS","AMAT","MSI","SYMC","ATVI","A","NTAP","CA","TDC","CERN","LNKD","ARMH","CBE","ADI","STX","ALTR","RHT",
              "SNDK","NOK","FISV","WDC","ROP","ROK","APH","CHKP","NJ","XRX","JNPR","CTRX","EQIX","XLNX","LPL","RAX","KLAC",
              "AVGO","AME","GRMN","NVDA","MXIM","LLTC","NUAN","FFIV","VRSN","YNDX","WAT","ADSK","GIB","BMC","AKAM","ANSS",
              "MCHP","MU","TRMB","NXPI","ASX","MRVL","SWKS","ILMN","DOX","HRS","MTD","STM","UMC","CSC","TIBX","SNPS","MOLX",
              "VNTV","AVT","JBL","IACI","ARBA","ENR","MOLXA","PANW","FDS","EA","LSI","TSS","MLNX","SWI","FTNT","FLEX","SINA",
              "SAI","MCRS","NOW","JAH","LRCX","CNQR","N","MR","ARW","INFA","RIMM","DNB","CDNS","NCR","MELI","SPIL","SPLK",
              "PAY","RVBD","PKI","ATHN","OTEX","IPGP","JKHY","CREE","NATI","AUO","AOL","SLH","BIO","ONNN","BWC","TER","JDSU",
              "AYI","PBI","BRCD","QIHU","CRUS","ATML","CAE","FIO","ALU","CLGX","ULTI","PMTC","WCC","AMD","SATS","CPHD","AOS",
              "FSL","SYNT","ATE","LPS","CVLT","IM","DST","HXL","AZPN","CSGP","LORL","CPWR","DDD","DBD","ZNGA","BRKR","AXE",
              "YOKU","FEIC","SPB","QLIK","ASMI","PANL","MDRX","NICE","AWAY","FSLR","MENT","PLCM","RP","SSNC","TECD","CY","ENS",
              "HRC","FCS","MSCC","CVG","ITRI","CYMI","AVX","ACIW","BDC","HITT","VSAT","CAVM","SMTC","VSH","CLS","P","FIRE",
              "CGNX","GWRE","ALR","SLAB","ARRS","LOGI","SOHU","PLT","LXK","ET","BGC","AIXG","FICO","GNRC","IDCC","SAPE","MKSI",
              "OSIS","VHC","RENN","FNSR","MSTR","VECO","CIEN","CSOD","ACXM","CMVT","TLAB","CYOU","JDAS","GTI","FELE",
              "SSYS","ACOM","SNX","INXN","APKT","PMCS","PRGS","ADTN","KNXA","IRF","TYL","CACI","ADVS","IIVI","KYAK","GA","TRAK",
              "QLGC","LFUS","ISIL","UBNT","COHR","PLXS","RFMD","QSII","VRNT","BLKB","IART","PEGA","NTCT","XXIA","MANH",
              "BSFT","SFUN","INVN","MDAS","POWI","ESE","EGOV","TTWO","LPSN","CCOI","MDSO","TQNT","TTEC","EZCH","SPRD","UIS",
              "SYNA","SNCR","IGTE","IMOS","BHE","BLOX","EBIX","PROJ","JIVE","ASIA","SCSC","OVTI","TTMI","ALOG","AZZ","DIOD",
              "DMD","UTEK","MTSC","ALLT","LMNX","EPAY","NSIT","MANT","WWWW","IDTI","DWRE","CCMP","ACCO","TSRA","EFII","AMKR",
              "PRLB","SPWR","OPNT","THR","HSTM","IMPV","ELNK","EPAM","GTAT","SANM","WBMD","CODE","PWER","ACTV","WFR","CSGS",
              "MPWR","RDWR","SREV","OYOG","ROG","FARO","VCRA","ELX","ELLI","BCOR","MCRL","ANGI","MG","ATMI","CTCT","AMAP",
              "SYKE","VLTR","DRIV","WBSN","CPSI","AVG","ININ","LOGM","LDR","DSGX","ARX","PKE","HLIT","TYPE","SMCI","MFLX",
              "SONS","ENTR","WNS","PWRD","TNGO","MX","PSMI","HOLI","BRKS","VELT","RMBS","ASEI","CALL","CMTL","BMI","MEAS",
              "AEIS","MTSI","PRO","RTLX","RDA","LSCC","OMCL","MTRN","SIFY","EPIQ","MKTG","PGI","CRAY","NTE","SIMO","KBALB",
              "POWL","ELOQ","PFPT","BBOX","GWAY","ACCL","PKT","VRTU","SCMR","SPSC","CASS","SIMG","FN","DAKT","VOCS","IN","QTM",
              "MIPS","INAP","CEVA","ESIO","VDSI","ORBK","PRFT","MEI","PDFS","NANO","EXAR","AUTH","VIT","IQNT","HSFT","STEC",
              "NQ","BIRT","AMCC","CTS","IPHI","EXTR","ZIGO","TSL","RTEC","MOVE","IL","FLDM","GLUU","MRGE","OPLK","IXYS","EXFO",
              "VIAS","MRCY","OCZ","BCOV","GCOM","HIMX","ZOLT","XRTX","SGI","LTXC","TASR","ISS","DGII","ISSI","CALX","GUID",
              "PLUS","IMI","YGE","NVEC","RST","FORM","PLXT","AFFX","SABA","VICR","SYMM","SEAC","GNMK","WIFI","SWIR","CBR",
              "XIDE","KOPN","KEYN","CKSW","MXL","AOSL","AQ","SHOR","SAAS","EPOC","IRIS","CRNT","COHU","BELFB","AMSWA","SUPX",
              "CAMP","QADA","TTGT","SIGM","RBCN","SYNC","AMSC","KEM","VRNG","PSEM","NVMI","PRKR","INTX","HWCC","SMT","AGYS",
              "ENPH","RELL","CARB","VPG","DATE","VOXX","TSEM","KITD","MGIC","CALD","LDK","PMFG","ATNY","NPTN","ACFN","ZIXI",
              "TESS","WRLS","SPRT","GILT","UPIP","DMRC","MLAB","IMMR","EXA","DTLK","OCLR","IVAC","UCTT")
spx<-as.data.frame(coredata(dailyReturn(getSymbols("^GSPC",auto.assign=FALSE,src="yahoo",verbose=FALSE,from=start))))

stock_pickedT=c(stock_list[1])
quote<-getSymbols(stock_list[1],auto.assign=FALSE,src="yahoo",verbose=FALSE,from=start)
qret<-as.data.frame(coredata(dailyReturn(quote)))
betaT<-c((cov(spx,qret)/var(spx))[1,1])
dataT<-as.data.frame(coredata(Cl(quote)))

dl<-dim(dataT)[1]
for(j in 2:length(stock_list))
{
  quote<-getSymbols(stock_list[j],auto.assign=FALSE,src="yahoo",verbose=FALSE,from=start)
  x<-as.data.frame(coredata(Cl(quote)))
  if(dim(x)[1]==dl)
  {
    qret<-as.data.frame(coredata(dailyReturn(quote)))
    beta<-(cov(spx,qret)/var(spx))[1,1]
    dataT<-cbind(dataT,x)
    betaT<-c(betaT,beta)
    stock_pickedT<-c(stock_pickedT,stock_list[j])
  }
}

numpairs=0
first=c()
second=c()
potret=c()
johcoeff=c()
sink("D:\\LiC\\stocks\\techs.txt")
for(i in 1:length(stock_pickedT))
{
  for(j in 1:length(stock_pickedT))
  {
    if(j>i)
    {
      johtest<-ca.jo(as.data.frame(t(rbind(t(dataT[1:dl,i]),t(dataT[1:dl,j])))),type="eigen",ecdet="none",K=2,spec="longrun")
      if(johtest@teststat[1]<johtest@cval[1,3]&&johtest@teststat[2]>johtest@cval[2,3]&&johtest@V[2,1]<0.0&&johtest@V[2,1]>-5.0)
      {
        v<-sqrt(var(dataT[range:dl,i]+johtest@V[2,1]*dataT[range:dl,j]))
        m<-mean(dataT[range:dl,i]+johtest@V[2,1]*dataT[range:dl,j])
        bp<-((betaT[i]*dataT[dl,i]+betaT[j]*johtest@V[2,1]*dataT[dl,j])/(dataT[dl,i]-johtest@V[2,1]*dataT[dl,j]))
        if(dataT[dl,i]+johtest@V[2,1]*dataT[dl,j]<m-2*v)
        {
          first<-c(first,stock_pickedT[i])
          second<-c(second,stock_pickedT[j])
          potret<-c(potret,2*v/dataT[dl,i])
          johcoeff<-c(johcoeff,johtest@V[2,1])
          numpairs=numpairs+1
        }
        if(dataT[dl,i]+johtest@V[2,1]*dataT[dl,j]>m+2*v)
        {
          first<-c(first,stock_pickedT[i])
          second<-c(second,stock_pickedT[j])
          potret<-c(potret,2*v/dataT[dl,j]/abs(johtest@V[2,1]))
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
  i<-which(stock_pickedT==first[pair])
  j<-which(stock_pickedT==second[pair])
  pairdata<-dataT[1:dl,i]+johcoeff[pair]*dataT[1:dl,j]
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
  i<-which(stock_pickedT==firstsorted[k])
  j<-which(stock_pickedT==secondsorted[k])
  v<-sqrt(var(dataT[range:dl,i]+johcoeffsorted[k]*dataT[range:dl,j]))
  v1<-sqrt(var(dataT[1:dl,i]+johcoeffsorted[k]*dataT[1:dl,j]))
  m<-mean(dataT[range:dl,i]+johcoeffsorted[k]*dataT[range:dl,j])
  m1<-mean(dataT[1:dl,i]+johcoeffsorted[k]*dataT[1:dl,j])
  print("Half life")
  print(hlsorted[k])
  print("Approx. mean-reversion time")
  print(meancyclesorted[k])
  print("Johansen coefficient")
  print(johcoeffsorted[k])
  print("Correlation")
  print(cor(dataT[1:dl,i],dataT[1:dl,j]))
  print("Possible return")
  print(potretsorted[k])
  print("****************************")
  plot(dataT[1:dl,i]+johcoeffsorted[k]*dataT[1:dl,j],type="l",main=paste(firstsorted[k],secondsorted[k]),xlab="",ylab="")
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
