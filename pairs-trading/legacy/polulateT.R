add_stock<-function(x,con,sec,i){
  open<-coredata(Op(x)['2010::'])
  close<-coredata(Cl(x)['2010::'])
  high<-coredata(Hi(x)['2010::'])
  low<-coredata(Lo(x)['2010::'])
  volume<-coredata(Vo(x)['2010::'])
  date<-as.data.frame(as.Date(index(Op(x)['2010::'])))
  ticker<-rep(stock_list[i],length(open))
  sector<-rep(sec,length(open))
  data<-as.data.frame(cbind(date,open,high,low,close,volume,ticker,sector))
  colnames(data)<-c("date","open","high","low","close","volume","ticker","sector")
  dbWriteTable(con, "price", as.data.frame(data),append=T,row.names=F)
  return (i+1)
}
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

for(j in 1:length(stock_list))
{
  getSymbols(stock_list[j],src="yahoo")
}

drv = dbDriver("MySQL")
con = dbConnect(drv,dbname="stockdata",user="root",pass="password")
dbGetQuery(con, "delete from price")
i=1

x<-AAPL
i<-add_stock(x,con,sec,i)

x<-MSFT
i<-add_stock(x,con,sec,i)

x<-IBM
i<-add_stock(x,con,sec,i)

x<-GOOG
i<-add_stock(x,con,sec,i)

x<-ORCL
i<-add_stock(x,con,sec,i)

x<-INTC
i<-add_stock(x,con,sec,i)

x<-QCOM
i<-add_stock(x,con,sec,i)

x<-CSCO
i<-add_stock(x,con,sec,i)

x<-SAP
i<-add_stock(x,con,sec,i)

x<-TSM
i<-add_stock(x,con,sec,i)

x<-EMC
i<-add_stock(x,con,sec,i)

x<-HON
i<-add_stock(x,con,sec,i)

x<-ABB
i<-add_stock(x,con,sec,i)

x<-VMW
i<-add_stock(x,con,sec,i)

x<-CAJ
i<-add_stock(x,con,sec,i)

x<-BIDU
i<-add_stock(x,con,sec,i)

x<-DHR
i<-add_stock(x,con,sec,i)

x<-EMR
i<-add_stock(x,con,sec,i)

x<-HPQ
i<-add_stock(x,con,sec,i)

x<-TXN
i<-add_stock(x,con,sec,i)

x<-ERIC
i<-add_stock(x,con,sec,i)

x<-INFY
i<-add_stock(x,con,sec,i)

x<-ASML
i<-add_stock(x,con,sec,i)

x<-CRM
i<-add_stock(x,con,sec,i)

x<-WIT
i<-add_stock(x,con,sec,i)

x<-CTSH
i<-add_stock(x,con,sec,i)

x<-BRCM
i<-add_stock(x,con,sec,i)

x<-GLW
i<-add_stock(x,con,sec,i)

x<-DELL
i<-add_stock(x,con,sec,i)

x<-INTU
i<-add_stock(x,con,sec,i)

x<-ETN
i<-add_stock(x,con,sec,i)

x<-ADBE
i<-add_stock(x,con,sec,i)

x<-TEL
i<-add_stock(x,con,sec,i)

x<-KYO
i<-add_stock(x,con,sec,i)

x<-CTXS
i<-add_stock(x,con,sec,i)

x<-AMAT
i<-add_stock(x,con,sec,i)

x<-MSI
i<-add_stock(x,con,sec,i)

x<-SYMC
i<-add_stock(x,con,sec,i)

x<-ATVI
i<-add_stock(x,con,sec,i)

x<-A
i<-add_stock(x,con,sec,i)

x<-NTAP
i<-add_stock(x,con,sec,i)

x<-CA
i<-add_stock(x,con,sec,i)

x<-TDC
i<-add_stock(x,con,sec,i)

x<-CERN
i<-add_stock(x,con,sec,i)

x<-LNKD
i<-add_stock(x,con,sec,i)

x<-ARMH
i<-add_stock(x,con,sec,i)

x<-CBE
i<-add_stock(x,con,sec,i)

x<-ADI
i<-add_stock(x,con,sec,i)

x<-STX
i<-add_stock(x,con,sec,i)

x<-ALTR
i<-add_stock(x,con,sec,i)

x<-RHT
i<-add_stock(x,con,sec,i)

x<-SNDK
i<-add_stock(x,con,sec,i)

x<-NOK
i<-add_stock(x,con,sec,i)

x<-FISV
i<-add_stock(x,con,sec,i)

x<-WDC
i<-add_stock(x,con,sec,i)

x<-ROP
i<-add_stock(x,con,sec,i)

x<-ROK
i<-add_stock(x,con,sec,i)

x<-APH
i<-add_stock(x,con,sec,i)

x<-CHKP
i<-add_stock(x,con,sec,i)

x<-NJ
i<-add_stock(x,con,sec,i)

x<-XRX
i<-add_stock(x,con,sec,i)

x<-JNPR
i<-add_stock(x,con,sec,i)

x<-CTRX
i<-add_stock(x,con,sec,i)

x<-EQIX
i<-add_stock(x,con,sec,i)

x<-XLNX
i<-add_stock(x,con,sec,i)

x<-LPL
i<-add_stock(x,con,sec,i)

x<-RAX
i<-add_stock(x,con,sec,i)

x<-KLAC
i<-add_stock(x,con,sec,i)

x<-AVGO
i<-add_stock(x,con,sec,i)

x<-AME
i<-add_stock(x,con,sec,i)

x<-GRMN
i<-add_stock(x,con,sec,i)

x<-NVDA
i<-add_stock(x,con,sec,i)

x<-MXIM
i<-add_stock(x,con,sec,i)

x<-LLTC
i<-add_stock(x,con,sec,i)

x<-NUAN
i<-add_stock(x,con,sec,i)

x<-FFIV
i<-add_stock(x,con,sec,i)

x<-VRSN
i<-add_stock(x,con,sec,i)

x<-YNDX
i<-add_stock(x,con,sec,i)

x<-WAT
i<-add_stock(x,con,sec,i)

x<-ADSK
i<-add_stock(x,con,sec,i)

x<-GIB
i<-add_stock(x,con,sec,i)

x<-BMC
i<-add_stock(x,con,sec,i)

x<-AKAM
i<-add_stock(x,con,sec,i)

x<-ANSS
i<-add_stock(x,con,sec,i)

x<-MCHP
i<-add_stock(x,con,sec,i)

x<-MU
i<-add_stock(x,con,sec,i)

x<-TRMB
i<-add_stock(x,con,sec,i)

x<-NXPI
i<-add_stock(x,con,sec,i)

x<-ASX
i<-add_stock(x,con,sec,i)

x<-MRVL
i<-add_stock(x,con,sec,i)

x<-SWKS
i<-add_stock(x,con,sec,i)

x<-ILMN
i<-add_stock(x,con,sec,i)

x<-DOX
i<-add_stock(x,con,sec,i)

x<-HRS
i<-add_stock(x,con,sec,i)

x<-MTD
i<-add_stock(x,con,sec,i)

x<-STM
i<-add_stock(x,con,sec,i)

x<-UMC
i<-add_stock(x,con,sec,i)

x<-CSC
i<-add_stock(x,con,sec,i)

x<-TIBX
i<-add_stock(x,con,sec,i)

x<-SNPS
i<-add_stock(x,con,sec,i)

x<-MOLX
i<-add_stock(x,con,sec,i)

x<-VNTV
i<-add_stock(x,con,sec,i)

x<-AVT
i<-add_stock(x,con,sec,i)

x<-JBL
i<-add_stock(x,con,sec,i)

x<-IACI
i<-add_stock(x,con,sec,i)

x<-ARBA
i<-add_stock(x,con,sec,i)

x<-ENR
i<-add_stock(x,con,sec,i)

x<-MOLXA
i<-add_stock(x,con,sec,i)

x<-PANW
i<-add_stock(x,con,sec,i)

x<-FDS
i<-add_stock(x,con,sec,i)

x<-EA
i<-add_stock(x,con,sec,i)

x<-LSI
i<-add_stock(x,con,sec,i)

x<-TSS
i<-add_stock(x,con,sec,i)

x<-MLNX
i<-add_stock(x,con,sec,i)

x<-SWI
i<-add_stock(x,con,sec,i)

x<-FTNT
i<-add_stock(x,con,sec,i)

x<-FLEX
i<-add_stock(x,con,sec,i)

x<-SINA
i<-add_stock(x,con,sec,i)

x<-SAI
i<-add_stock(x,con,sec,i)

x<-MCRS
i<-add_stock(x,con,sec,i)

x<-NOW
i<-add_stock(x,con,sec,i)

x<-JAH
i<-add_stock(x,con,sec,i)

x<-LRCX
i<-add_stock(x,con,sec,i)

x<-CNQR
i<-add_stock(x,con,sec,i)

x<-N
i<-add_stock(x,con,sec,i)

x<-MR
i<-add_stock(x,con,sec,i)

x<-ARW
i<-add_stock(x,con,sec,i)

x<-INFA
i<-add_stock(x,con,sec,i)

x<-RIMM
i<-add_stock(x,con,sec,i)

x<-DNB
i<-add_stock(x,con,sec,i)

x<-CDNS
i<-add_stock(x,con,sec,i)

x<-NCR
i<-add_stock(x,con,sec,i)

x<-MELI
i<-add_stock(x,con,sec,i)

x<-SPIL
i<-add_stock(x,con,sec,i)

x<-SPLK
i<-add_stock(x,con,sec,i)

x<-PAY
i<-add_stock(x,con,sec,i)

x<-RVBD
i<-add_stock(x,con,sec,i)

x<-PKI
i<-add_stock(x,con,sec,i)

x<-ATHN
i<-add_stock(x,con,sec,i)

x<-OTEX
i<-add_stock(x,con,sec,i)

x<-IPGP
i<-add_stock(x,con,sec,i)

x<-JKHY
i<-add_stock(x,con,sec,i)

x<-CREE
i<-add_stock(x,con,sec,i)

x<-NATI
i<-add_stock(x,con,sec,i)

x<-AUO
i<-add_stock(x,con,sec,i)

x<-AOL
i<-add_stock(x,con,sec,i)

x<-SLH
i<-add_stock(x,con,sec,i)

x<-BIO
i<-add_stock(x,con,sec,i)

x<-ONNN
i<-add_stock(x,con,sec,i)

x<-BWC
i<-add_stock(x,con,sec,i)

x<-TER
i<-add_stock(x,con,sec,i)

x<-JDSU
i<-add_stock(x,con,sec,i)

x<-AYI
i<-add_stock(x,con,sec,i)

x<-PBI
i<-add_stock(x,con,sec,i)

x<-BRCD
i<-add_stock(x,con,sec,i)

x<-QIHU
i<-add_stock(x,con,sec,i)

x<-CRUS
i<-add_stock(x,con,sec,i)

x<-ATML
i<-add_stock(x,con,sec,i)

x<-CAE
i<-add_stock(x,con,sec,i)

x<-FIO
i<-add_stock(x,con,sec,i)

x<-ALU
i<-add_stock(x,con,sec,i)

x<-CLGX
i<-add_stock(x,con,sec,i)

x<-ULTI
i<-add_stock(x,con,sec,i)

x<-PMTC
i<-add_stock(x,con,sec,i)

x<-WCC
i<-add_stock(x,con,sec,i)

x<-AMD
i<-add_stock(x,con,sec,i)

x<-SATS
i<-add_stock(x,con,sec,i)

x<-CPHD
i<-add_stock(x,con,sec,i)

x<-AOS
i<-add_stock(x,con,sec,i)

x<-FSL
i<-add_stock(x,con,sec,i)

x<-SYNT
i<-add_stock(x,con,sec,i)

x<-ATE
i<-add_stock(x,con,sec,i)

x<-LPS
i<-add_stock(x,con,sec,i)

x<-CVLT
i<-add_stock(x,con,sec,i)

x<-IM
i<-add_stock(x,con,sec,i)

x<-DST
i<-add_stock(x,con,sec,i)

x<-HXL
i<-add_stock(x,con,sec,i)

x<-AZPN
i<-add_stock(x,con,sec,i)

x<-CSGP
i<-add_stock(x,con,sec,i)

x<-LORL
i<-add_stock(x,con,sec,i)

x<-CPWR
i<-add_stock(x,con,sec,i)

x<-DDD
i<-add_stock(x,con,sec,i)

x<-DBD
i<-add_stock(x,con,sec,i)

x<-ZNGA
i<-add_stock(x,con,sec,i)

x<-BRKR
i<-add_stock(x,con,sec,i)

x<-AXE
i<-add_stock(x,con,sec,i)

x<-YOKU
i<-add_stock(x,con,sec,i)

x<-FEIC
i<-add_stock(x,con,sec,i)

x<-SPB
i<-add_stock(x,con,sec,i)

x<-QLIK
i<-add_stock(x,con,sec,i)

x<-ASMI
i<-add_stock(x,con,sec,i)

x<-PANL
i<-add_stock(x,con,sec,i)

x<-MDRX
i<-add_stock(x,con,sec,i)

x<-NICE
i<-add_stock(x,con,sec,i)

x<-AWAY
i<-add_stock(x,con,sec,i)

x<-FSLR
i<-add_stock(x,con,sec,i)

x<-MENT
i<-add_stock(x,con,sec,i)

x<-PLCM
i<-add_stock(x,con,sec,i)

x<-RP
i<-add_stock(x,con,sec,i)

x<-SSNC
i<-add_stock(x,con,sec,i)

x<-TECD
i<-add_stock(x,con,sec,i)

x<-CY
i<-add_stock(x,con,sec,i)

x<-ENS
i<-add_stock(x,con,sec,i)

x<-HRC
i<-add_stock(x,con,sec,i)

x<-FCS
i<-add_stock(x,con,sec,i)

x<-MSCC
i<-add_stock(x,con,sec,i)

x<-CVG
i<-add_stock(x,con,sec,i)

x<-ITRI
i<-add_stock(x,con,sec,i)

x<-CYMI
i<-add_stock(x,con,sec,i)

x<-AVX
i<-add_stock(x,con,sec,i)

x<-ACIW
i<-add_stock(x,con,sec,i)

x<-BDC
i<-add_stock(x,con,sec,i)

x<-HITT
i<-add_stock(x,con,sec,i)

x<-VSAT
i<-add_stock(x,con,sec,i)

x<-CAVM
i<-add_stock(x,con,sec,i)

x<-SMTC
i<-add_stock(x,con,sec,i)

x<-VSH
i<-add_stock(x,con,sec,i)

x<-CLS
i<-add_stock(x,con,sec,i)

x<-P
i<-add_stock(x,con,sec,i)

x<-FIRE
i<-add_stock(x,con,sec,i)

x<-CGNX
i<-add_stock(x,con,sec,i)

x<-GWRE
i<-add_stock(x,con,sec,i)

x<-ALR
i<-add_stock(x,con,sec,i)

x<-SLAB
i<-add_stock(x,con,sec,i)

x<-ARRS
i<-add_stock(x,con,sec,i)

x<-LOGI
i<-add_stock(x,con,sec,i)

x<-SOHU
i<-add_stock(x,con,sec,i)

x<-PLT
i<-add_stock(x,con,sec,i)

x<-LXK
i<-add_stock(x,con,sec,i)

x<-ET
i<-add_stock(x,con,sec,i)

x<-BGC
i<-add_stock(x,con,sec,i)

x<-AIXG
i<-add_stock(x,con,sec,i)

x<-FICO
i<-add_stock(x,con,sec,i)

x<-GNRC
i<-add_stock(x,con,sec,i)

x<-IDCC
i<-add_stock(x,con,sec,i)

x<-SAPE
i<-add_stock(x,con,sec,i)

x<-MKSI
i<-add_stock(x,con,sec,i)

x<-OSIS
i<-add_stock(x,con,sec,i)

x<-VHC
i<-add_stock(x,con,sec,i)

x<-RENN
i<-add_stock(x,con,sec,i)

x<-FNSR
i<-add_stock(x,con,sec,i)

x<-MSTR
i<-add_stock(x,con,sec,i)

x<-VECO
i<-add_stock(x,con,sec,i)

x<-CIEN
i<-add_stock(x,con,sec,i)

x<-CSOD
i<-add_stock(x,con,sec,i)

x<-ACXM
i<-add_stock(x,con,sec,i)

x<-CMVT
i<-add_stock(x,con,sec,i)

x<-TLAB
i<-add_stock(x,con,sec,i)

x<-CYOU
i<-add_stock(x,con,sec,i)

x<-JDAS
i<-add_stock(x,con,sec,i)

x<-GTI
i<-add_stock(x,con,sec,i)

x<-FELE
i<-add_stock(x,con,sec,i)

x<-SSYS
i<-add_stock(x,con,sec,i)

x<-ACOM
i<-add_stock(x,con,sec,i)

x<-SNX
i<-add_stock(x,con,sec,i)

x<-INXN
i<-add_stock(x,con,sec,i)

x<-APKT
i<-add_stock(x,con,sec,i)

x<-PMCS
i<-add_stock(x,con,sec,i)

x<-PRGS
i<-add_stock(x,con,sec,i)

x<-ADTN
i<-add_stock(x,con,sec,i)

x<-KNXA
i<-add_stock(x,con,sec,i)

x<-IRF
i<-add_stock(x,con,sec,i)

x<-TYL
i<-add_stock(x,con,sec,i)

x<-CACI
i<-add_stock(x,con,sec,i)

x<-ADVS
i<-add_stock(x,con,sec,i)

x<-IIVI
i<-add_stock(x,con,sec,i)

x<-KYAK
i<-add_stock(x,con,sec,i)

x<-GA
i<-add_stock(x,con,sec,i)

x<-TRAK
i<-add_stock(x,con,sec,i)

x<-QLGC
i<-add_stock(x,con,sec,i)

x<-LFUS
i<-add_stock(x,con,sec,i)

x<-ISIL
i<-add_stock(x,con,sec,i)

x<-UBNT
i<-add_stock(x,con,sec,i)

x<-COHR
i<-add_stock(x,con,sec,i)

x<-PLXS
i<-add_stock(x,con,sec,i)

x<-RFMD
i<-add_stock(x,con,sec,i)

x<-QSII
i<-add_stock(x,con,sec,i)

x<-VRNT
i<-add_stock(x,con,sec,i)

x<-BLKB
i<-add_stock(x,con,sec,i)

x<-IART
i<-add_stock(x,con,sec,i)

x<-PEGA
i<-add_stock(x,con,sec,i)

x<-NTCT
i<-add_stock(x,con,sec,i)

x<-XXIA
i<-add_stock(x,con,sec,i)

x<-MANH
i<-add_stock(x,con,sec,i)

x<-BSFT
i<-add_stock(x,con,sec,i)

x<-SFUN
i<-add_stock(x,con,sec,i)

x<-INVN
i<-add_stock(x,con,sec,i)

x<-MDAS
i<-add_stock(x,con,sec,i)

x<-POWI
i<-add_stock(x,con,sec,i)

x<-ESE
i<-add_stock(x,con,sec,i)

x<-EGOV
i<-add_stock(x,con,sec,i)

x<-TTWO
i<-add_stock(x,con,sec,i)

x<-LPSN
i<-add_stock(x,con,sec,i)

x<-CCOI
i<-add_stock(x,con,sec,i)

x<-MDSO
i<-add_stock(x,con,sec,i)

x<-TQNT
i<-add_stock(x,con,sec,i)

x<-TTEC
i<-add_stock(x,con,sec,i)

x<-EZCH
i<-add_stock(x,con,sec,i)

x<-SPRD
i<-add_stock(x,con,sec,i)

x<-UIS
i<-add_stock(x,con,sec,i)

x<-SYNA
i<-add_stock(x,con,sec,i)

x<-SNCR
i<-add_stock(x,con,sec,i)

x<-IGTE
i<-add_stock(x,con,sec,i)

x<-IMOS
i<-add_stock(x,con,sec,i)

x<-BHE
i<-add_stock(x,con,sec,i)

x<-BLOX
i<-add_stock(x,con,sec,i)

x<-EBIX
i<-add_stock(x,con,sec,i)

x<-PROJ
i<-add_stock(x,con,sec,i)

x<-JIVE
i<-add_stock(x,con,sec,i)

x<-ASIA
i<-add_stock(x,con,sec,i)

x<-SCSC
i<-add_stock(x,con,sec,i)

x<-OVTI
i<-add_stock(x,con,sec,i)

x<-TTMI
i<-add_stock(x,con,sec,i)

x<-ALOG
i<-add_stock(x,con,sec,i)

x<-AZZ
i<-add_stock(x,con,sec,i)

x<-DIOD
i<-add_stock(x,con,sec,i)

x<-DMD
i<-add_stock(x,con,sec,i)

x<-UTEK
i<-add_stock(x,con,sec,i)

x<-MTSC
i<-add_stock(x,con,sec,i)

x<-ALLT
i<-add_stock(x,con,sec,i)

x<-LMNX
i<-add_stock(x,con,sec,i)

x<-EPAY
i<-add_stock(x,con,sec,i)

x<-NSIT
i<-add_stock(x,con,sec,i)

x<-MANT
i<-add_stock(x,con,sec,i)

x<-WWWW
i<-add_stock(x,con,sec,i)

x<-IDTI
i<-add_stock(x,con,sec,i)

x<-DWRE
i<-add_stock(x,con,sec,i)

x<-CCMP
i<-add_stock(x,con,sec,i)

x<-ACCO
i<-add_stock(x,con,sec,i)

x<-TSRA
i<-add_stock(x,con,sec,i)

x<-EFII
i<-add_stock(x,con,sec,i)

x<-AMKR
i<-add_stock(x,con,sec,i)

x<-PRLB
i<-add_stock(x,con,sec,i)

x<-SPWR
i<-add_stock(x,con,sec,i)

x<-OPNT
i<-add_stock(x,con,sec,i)

x<-THR
i<-add_stock(x,con,sec,i)

x<-HSTM
i<-add_stock(x,con,sec,i)

x<-IMPV
i<-add_stock(x,con,sec,i)

x<-ELNK
i<-add_stock(x,con,sec,i)

x<-EPAM
i<-add_stock(x,con,sec,i)

x<-GTAT
i<-add_stock(x,con,sec,i)

x<-SANM
i<-add_stock(x,con,sec,i)

x<-WBMD
i<-add_stock(x,con,sec,i)

x<-CODE
i<-add_stock(x,con,sec,i)

x<-PWER
i<-add_stock(x,con,sec,i)

x<-ACTV
i<-add_stock(x,con,sec,i)

x<-WFR
i<-add_stock(x,con,sec,i)

x<-CSGS
i<-add_stock(x,con,sec,i)

x<-MPWR
i<-add_stock(x,con,sec,i)

x<-RDWR
i<-add_stock(x,con,sec,i)

x<-SREV
i<-add_stock(x,con,sec,i)

x<-OYOG
i<-add_stock(x,con,sec,i)

x<-ROG
i<-add_stock(x,con,sec,i)

x<-FARO
i<-add_stock(x,con,sec,i)

x<-VCRA
i<-add_stock(x,con,sec,i)

x<-ELX
i<-add_stock(x,con,sec,i)

x<-ELLI
i<-add_stock(x,con,sec,i)

x<-BCOR
i<-add_stock(x,con,sec,i)

x<-MCRL
i<-add_stock(x,con,sec,i)

x<-ANGI
i<-add_stock(x,con,sec,i)

x<-MG
i<-add_stock(x,con,sec,i)

x<-ATMI
i<-add_stock(x,con,sec,i)

x<-CTCT
i<-add_stock(x,con,sec,i)

x<-AMAP
i<-add_stock(x,con,sec,i)

x<-SYKE
i<-add_stock(x,con,sec,i)

x<-VLTR
i<-add_stock(x,con,sec,i)

x<-DRIV
i<-add_stock(x,con,sec,i)

x<-WBSN
i<-add_stock(x,con,sec,i)

x<-CPSI
i<-add_stock(x,con,sec,i)

x<-AVG
i<-add_stock(x,con,sec,i)

x<-ININ
i<-add_stock(x,con,sec,i)

x<-LOGM
i<-add_stock(x,con,sec,i)

x<-LDR
i<-add_stock(x,con,sec,i)

x<-DSGX
i<-add_stock(x,con,sec,i)

x<-ARX
i<-add_stock(x,con,sec,i)

x<-PKE
i<-add_stock(x,con,sec,i)

x<-HLIT
i<-add_stock(x,con,sec,i)

x<-TYPE
i<-add_stock(x,con,sec,i)

x<-SMCI
i<-add_stock(x,con,sec,i)

x<-MFLX
i<-add_stock(x,con,sec,i)

x<-SONS
i<-add_stock(x,con,sec,i)

x<-ENTR
i<-add_stock(x,con,sec,i)

x<-WNS
i<-add_stock(x,con,sec,i)

x<-PWRD
i<-add_stock(x,con,sec,i)

x<-TNGO
i<-add_stock(x,con,sec,i)

x<-MX
i<-add_stock(x,con,sec,i)

x<-PSMI
i<-add_stock(x,con,sec,i)

x<-HOLI
i<-add_stock(x,con,sec,i)

x<-BRKS
i<-add_stock(x,con,sec,i)

x<-VELT
i<-add_stock(x,con,sec,i)

x<-RMBS
i<-add_stock(x,con,sec,i)

x<-ASEI
i<-add_stock(x,con,sec,i)

x<-CALL
i<-add_stock(x,con,sec,i)

x<-CMTL
i<-add_stock(x,con,sec,i)

x<-BMI
i<-add_stock(x,con,sec,i)

x<-MEAS
i<-add_stock(x,con,sec,i)

x<-AEIS
i<-add_stock(x,con,sec,i)

x<-MTSI
i<-add_stock(x,con,sec,i)

x<-PRO
i<-add_stock(x,con,sec,i)

x<-RTLX
i<-add_stock(x,con,sec,i)

x<-RDA
i<-add_stock(x,con,sec,i)

x<-LSCC
i<-add_stock(x,con,sec,i)

x<-OMCL
i<-add_stock(x,con,sec,i)

x<-MTRN
i<-add_stock(x,con,sec,i)

x<-SIFY
i<-add_stock(x,con,sec,i)

x<-EPIQ
i<-add_stock(x,con,sec,i)

x<-MKTG
i<-add_stock(x,con,sec,i)

x<-PGI
i<-add_stock(x,con,sec,i)

x<-CRAY
i<-add_stock(x,con,sec,i)

x<-NTE
i<-add_stock(x,con,sec,i)

x<-SIMO
i<-add_stock(x,con,sec,i)

x<-KBALB
i<-add_stock(x,con,sec,i)

x<-POWL
i<-add_stock(x,con,sec,i)

x<-ELOQ
i<-add_stock(x,con,sec,i)

x<-PFPT
i<-add_stock(x,con,sec,i)

x<-BBOX
i<-add_stock(x,con,sec,i)

x<-GWAY
i<-add_stock(x,con,sec,i)

x<-ACCL
i<-add_stock(x,con,sec,i)

x<-PKT
i<-add_stock(x,con,sec,i)

x<-VRTU
i<-add_stock(x,con,sec,i)

x<-SCMR
i<-add_stock(x,con,sec,i)

x<-SPSC
i<-add_stock(x,con,sec,i)

x<-CASS
i<-add_stock(x,con,sec,i)

x<-SIMG
i<-add_stock(x,con,sec,i)

x<-FN
i<-add_stock(x,con,sec,i)

x<-DAKT
i<-add_stock(x,con,sec,i)

x<-VOCS
i<-add_stock(x,con,sec,i)

x<-IN
i<-add_stock(x,con,sec,i)

x<-QTM
i<-add_stock(x,con,sec,i)

x<-MIPS
i<-add_stock(x,con,sec,i)

x<-INAP
i<-add_stock(x,con,sec,i)

x<-CEVA
i<-add_stock(x,con,sec,i)

x<-ESIO
i<-add_stock(x,con,sec,i)

x<-VDSI
i<-add_stock(x,con,sec,i)

x<-ORBK
i<-add_stock(x,con,sec,i)

x<-PRFT
i<-add_stock(x,con,sec,i)

x<-MEI
i<-add_stock(x,con,sec,i)

x<-PDFS
i<-add_stock(x,con,sec,i)

x<-NANO
i<-add_stock(x,con,sec,i)

x<-EXAR
i<-add_stock(x,con,sec,i)

x<-AUTH
i<-add_stock(x,con,sec,i)

x<-VIT
i<-add_stock(x,con,sec,i)

x<-IQNT
i<-add_stock(x,con,sec,i)

x<-HSFT
i<-add_stock(x,con,sec,i)

x<-STEC
i<-add_stock(x,con,sec,i)

x<-NQ
i<-add_stock(x,con,sec,i)

x<-BIRT
i<-add_stock(x,con,sec,i)

x<-AMCC
i<-add_stock(x,con,sec,i)

x<-CTS
i<-add_stock(x,con,sec,i)

x<-IPHI
i<-add_stock(x,con,sec,i)

x<-EXTR
i<-add_stock(x,con,sec,i)

x<-ZIGO
i<-add_stock(x,con,sec,i)

x<-TSL
i<-add_stock(x,con,sec,i)

x<-RTEC
i<-add_stock(x,con,sec,i)

x<-MOVE
i<-add_stock(x,con,sec,i)

x<-IL
i<-add_stock(x,con,sec,i)

x<-FLDM
i<-add_stock(x,con,sec,i)

x<-GLUU
i<-add_stock(x,con,sec,i)

x<-MRGE
i<-add_stock(x,con,sec,i)

x<-OPLK
i<-add_stock(x,con,sec,i)

x<-IXYS
i<-add_stock(x,con,sec,i)

x<-EXFO
i<-add_stock(x,con,sec,i)

x<-VIAS
i<-add_stock(x,con,sec,i)

x<-MRCY
i<-add_stock(x,con,sec,i)

x<-OCZ
i<-add_stock(x,con,sec,i)

x<-BCOV
i<-add_stock(x,con,sec,i)

x<-GCOM
i<-add_stock(x,con,sec,i)

x<-HIMX
i<-add_stock(x,con,sec,i)

x<-ZOLT
i<-add_stock(x,con,sec,i)

x<-XRTX
i<-add_stock(x,con,sec,i)

x<-SGI
i<-add_stock(x,con,sec,i)

x<-LTXC
i<-add_stock(x,con,sec,i)

x<-TASR
i<-add_stock(x,con,sec,i)

x<-ISS
i<-add_stock(x,con,sec,i)

x<-DGII
i<-add_stock(x,con,sec,i)

x<-ISSI
i<-add_stock(x,con,sec,i)

x<-CALX
i<-add_stock(x,con,sec,i)

x<-GUID
i<-add_stock(x,con,sec,i)

x<-PLUS
i<-add_stock(x,con,sec,i)

x<-IMI
i<-add_stock(x,con,sec,i)

x<-YGE
i<-add_stock(x,con,sec,i)

x<-NVEC
i<-add_stock(x,con,sec,i)

x<-RST
i<-add_stock(x,con,sec,i)

x<-FORM
i<-add_stock(x,con,sec,i)

x<-PLXT
i<-add_stock(x,con,sec,i)

x<-AFFX
i<-add_stock(x,con,sec,i)

x<-SABA
i<-add_stock(x,con,sec,i)

x<-VICR
i<-add_stock(x,con,sec,i)

x<-SYMM
i<-add_stock(x,con,sec,i)

x<-SEAC
i<-add_stock(x,con,sec,i)

x<-GNMK
i<-add_stock(x,con,sec,i)

x<-WIFI
i<-add_stock(x,con,sec,i)

x<-SWIR
i<-add_stock(x,con,sec,i)

x<-CBR
i<-add_stock(x,con,sec,i)

x<-XIDE
i<-add_stock(x,con,sec,i)

x<-KOPN
i<-add_stock(x,con,sec,i)

x<-KEYN
i<-add_stock(x,con,sec,i)

x<-CKSW
i<-add_stock(x,con,sec,i)

x<-MXL
i<-add_stock(x,con,sec,i)

x<-AOSL
i<-add_stock(x,con,sec,i)

x<-AQ
i<-add_stock(x,con,sec,i)

x<-SHOR
i<-add_stock(x,con,sec,i)

x<-SAAS
i<-add_stock(x,con,sec,i)

x<-EPOC
i<-add_stock(x,con,sec,i)

x<-IRIS
i<-add_stock(x,con,sec,i)

x<-CRNT
i<-add_stock(x,con,sec,i)

x<-COHU
i<-add_stock(x,con,sec,i)

x<-BELFB
i<-add_stock(x,con,sec,i)

x<-AMSWA
i<-add_stock(x,con,sec,i)

x<-SUPX
i<-add_stock(x,con,sec,i)

x<-CAMP
i<-add_stock(x,con,sec,i)

x<-QADA
i<-add_stock(x,con,sec,i)

x<-TTGT
i<-add_stock(x,con,sec,i)

x<-SIGM
i<-add_stock(x,con,sec,i)

x<-RBCN
i<-add_stock(x,con,sec,i)

x<-SYNC
i<-add_stock(x,con,sec,i)

x<-AMSC
i<-add_stock(x,con,sec,i)

x<-KEM
i<-add_stock(x,con,sec,i)

x<-VRNG
i<-add_stock(x,con,sec,i)

x<-PSEM
i<-add_stock(x,con,sec,i)

x<-NVMI
i<-add_stock(x,con,sec,i)

x<-PRKR
i<-add_stock(x,con,sec,i)

x<-INTX
i<-add_stock(x,con,sec,i)

x<-HWCC
i<-add_stock(x,con,sec,i)

x<-SMT
i<-add_stock(x,con,sec,i)

x<-AGYS
i<-add_stock(x,con,sec,i)

x<-ENPH
i<-add_stock(x,con,sec,i)

x<-RELL
i<-add_stock(x,con,sec,i)

x<-CARB
i<-add_stock(x,con,sec,i)

x<-VPG
i<-add_stock(x,con,sec,i)

x<-DATE
i<-add_stock(x,con,sec,i)

x<-VOXX
i<-add_stock(x,con,sec,i)

x<-TSEM
i<-add_stock(x,con,sec,i)

x<-KITD
i<-add_stock(x,con,sec,i)

x<-MGIC
i<-add_stock(x,con,sec,i)

x<-CALD
i<-add_stock(x,con,sec,i)

x<-LDK
i<-add_stock(x,con,sec,i)

x<-PMFG
i<-add_stock(x,con,sec,i)

x<-ATNY
i<-add_stock(x,con,sec,i)

x<-NPTN
i<-add_stock(x,con,sec,i)

x<-ACFN
i<-add_stock(x,con,sec,i)

x<-ZIXI
i<-add_stock(x,con,sec,i)

x<-TESS
i<-add_stock(x,con,sec,i)

x<-WRLS
i<-add_stock(x,con,sec,i)

x<-SPRT
i<-add_stock(x,con,sec,i)

x<-GILT
i<-add_stock(x,con,sec,i)

x<-UPIP
i<-add_stock(x,con,sec,i)

x<-DMRC
i<-add_stock(x,con,sec,i)

x<-MLAB
i<-add_stock(x,con,sec,i)

x<-IMMR
i<-add_stock(x,con,sec,i)

x<-EXA
i<-add_stock(x,con,sec,i)

x<-DTLK
i<-add_stock(x,con,sec,i)

x<-OCLR
i<-add_stock(x,con,sec,i)

x<-IVAC
i<-add_stock(x,con,sec,i)

x<-UCTT
i<-add_stock(x,con,sec,i)

dbDisconnect(con)