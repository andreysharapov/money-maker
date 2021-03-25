drv = dbDriver("MySQL")
con = dbConnect(drv,dbname="stockdata",user="root",pass="password")
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
stock_picked=c(stock_list[1])
dataT<-t(dbGetQuery(con, paste("select close from price where ticker=\'",stock_list[1],"\'",sep="")))
dl<-dim(dataT)[2]
for(i in 2:length(stock_list))
{
  x<-t(dbGetQuery(con, paste("select close from price where ticker=\'",stock_list[i],"\'",sep="")))
  if(dim(x)[2]==dl)
  {
    dataT<-rbind(dataT,x)
    stock_picked<-c(stock_picked,stock_list[i])
  }
  
}
dataT<-t(dataT)
dbDisconnect(con)

numpairs=0
sink("/home/quant/stocks/testresults")
for(i in 1:length(stock_picked))
{
  for(j in 1:length(stock_picked))
  {
    if(j>i)
    {
      johtest<-ca.jo(as.data.frame(t(rbind(t(dataT[1:dl,i]),t(dataT[1:dl,j])))),type="eigen",ecdet="none",K=2,spec="longrun")
      if(johtest@teststat[1]<johtest@cval[1,2]&&johtest@teststat[2]>johtest@cval[2,2]&&cor(dataT[1:dl,i],dataT[1:dl,j])>0.9)
      {
        v<-sqrt(var(dataT[1:dl,i]+johtest@V[2,1]*dataT[1:dl,j]))
        m<-mean(dataT[1:dl,i]+johtest@V[2,1]*dataT[1:dl,j])
        if((dataT[dl,i]+johtest@V[2,1]*dataT[dl,j]>m+2*v)||(dataT[dl,i]+johtest@V[2,1]*dataT[dl,j]<m-2*v))
        {
        print(stock_picked[i])
        print(i)
        print(stock_picked[j])
        print(j)
        print(johtest@V[1:2,1])
        print(cor(dataT[1:dl,i],dataT[1:dl,j]))
        print("****************************")
        plot(dataT[1:dl,i]+johtest@V[2,1]*dataT[1:dl,j],type="l",main=paste(stock_picked[i],stock_picked[j]))
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
      johtest<-ca.jo(as.data.frame(t(rbind(t(dataT[1:dl,i]),t(dataT[1:dl,j])))),type="eigen",ecdet="none",K=2,spec="longrun")
      if(johtest@teststat[1]<johtest@cval[1,2]&&johtest@teststat[2]>johtest@cval[2,2]&&cor(dataT[1:dl,i],dataT[1:dl,j])>0.9)
      {
        v<-sqrt(var(dataT[1:dl,i]+johtest@V[2,1]*dataT[1:dl,j]))
        m<-mean(dataT[1:dl,i]+johtest@V[2,1]*dataT[1:dl,j])
        if((dataT[dl,i]+johtest@V[2,1]*dataT[dl,j]>m+v)||(dataT[dl,i]+johtest@V[2,1]*dataT[dl,j]<m-v))
        {
          print(stock_picked[i])
          print(i)
          print(stock_picked[j])
          print(j)
          print(johtest@V[1:2,1])
          print(cor(dataT[1:dl,i],dataT[1:dl,j]))
          print("****************************")
          plot(dataT[1:dl,i]+johtest@V[2,1]*dataT[1:dl,j],type="l",main=paste(stock_picked[i],stock_picked[j]))
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