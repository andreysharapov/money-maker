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

for(j in 1:length(stock_list))
{
  getSymbols(stock_list[j],src="yahoo")
}

drv = dbDriver("MySQL")
con = dbConnect(drv,dbname="stockdata",user="root",pass="password")
dbGetQuery(con, "delete from price")
i=1

x<-BHP
i<-add_stock(x,con,sec,i)

x<-VALE
i<-add_stock(x,con,sec,i)

x<-RIO
i<-add_stock(x,con,sec,i)

x<-BBL
i<-add_stock(x,con,sec,i)

x<-MON
i<-add_stock(x,con,sec,i)

x<-DD
i<-add_stock(x,con,sec,i)

x<-ABX
i<-add_stock(x,con,sec,i)

x<-FCX
i<-add_stock(x,con,sec,i)

x<-DOW
i<-add_stock(x,con,sec,i)

x<-POT
i<-add_stock(x,con,sec,i)

x<-GG
i<-add_stock(x,con,sec,i)

x<-SYT
i<-add_stock(x,con,sec,i)

x<-PX
i<-add_stock(x,con,sec,i)

x<-SSL
i<-add_stock(x,con,sec,i)

x<-SCCO
i<-add_stock(x,con,sec,i)

x<-LYB
i<-add_stock(x,con,sec,i)

x<-NEM
i<-add_stock(x,con,sec,i)

x<-MOS
i<-add_stock(x,con,sec,i)

x<-PKX
i<-add_stock(x,con,sec,i)

x<-MT
i<-add_stock(x,con,sec,i)

x<-PCP
i<-add_stock(x,con,sec,i)

x<-TCK
i<-add_stock(x,con,sec,i)

x<-APD
i<-add_stock(x,con,sec,i)

x<-PPG
i<-add_stock(x,con,sec,i)

x<-GGB
i<-add_stock(x,con,sec,i)

x<-SQM
i<-add_stock(x,con,sec,i)

x<-AGU
i<-add_stock(x,con,sec,i)

x<-IP
i<-add_stock(x,con,sec,i)

x<-SHW
i<-add_stock(x,con,sec,i)

x<-WY
i<-add_stock(x,con,sec,i)

x<-CF
i<-add_stock(x,con,sec,i)

x<-AU
i<-add_stock(x,con,sec,i)

x<-AUY
i<-add_stock(x,con,sec,i)

x<-SLW
i<-add_stock(x,con,sec,i)

x<-PH
i<-add_stock(x,con,sec,i)

x<-NUE
i<-add_stock(x,con,sec,i)

x<-DOV
i<-add_stock(x,con,sec,i)

x<-EGO
i<-add_stock(x,con,sec,i)

x<-GOLD
i<-add_stock(x,con,sec,i)

x<-AA
i<-add_stock(x,con,sec,i)

x<-BVN
i<-add_stock(x,con,sec,i)

x<-GFI
i<-add_stock(x,con,sec,i)

x<-SIAL
i<-add_stock(x,con,sec,i)

x<-CCJ
i<-add_stock(x,con,sec,i)

x<-SID
i<-add_stock(x,con,sec,i)

x<-AEM
i<-add_stock(x,con,sec,i)

x<-FMC
i<-add_stock(x,con,sec,i)

x<-EMN
i<-add_stock(x,con,sec,i)

x<-TRQ
i<-add_stock(x,con,sec,i)

x<-PCL
i<-add_stock(x,con,sec,i)

x<-BLL
i<-add_stock(x,con,sec,i)

x<-ARG
i<-add_stock(x,con,sec,i)

x<-CE
i<-add_stock(x,con,sec,i)

x<-RYN
i<-add_stock(x,con,sec,i)

x<-BAK
i<-add_stock(x,con,sec,i)

x<-SLT
i<-add_stock(x,con,sec,i)

x<-CLF
i<-add_stock(x,con,sec,i)

x<-CCK
i<-add_stock(x,con,sec,i)

x<-LUK
i<-add_stock(x,con,sec,i)

x<-ACH
i<-add_stock(x,con,sec,i)

x<-IAG
i<-add_stock(x,con,sec,i)

x<-NGD
i<-add_stock(x,con,sec,i)

x<-RGLD
i<-add_stock(x,con,sec,i)

x<-MWV
i<-add_stock(x,con,sec,i)

x<-VAL
i<-add_stock(x,con,sec,i)

x<-ALB
i<-add_stock(x,con,sec,i)

x<-IFF
i<-add_stock(x,con,sec,i)

x<-RKT
i<-add_stock(x,con,sec,i)

x<-WLK
i<-add_stock(x,con,sec,i)

x<-GRA
i<-add_stock(x,con,sec,i)

x<-RS
i<-add_stock(x,con,sec,i)

x<-TNH
i<-add_stock(x,con,sec,i)

x<-FBR
i<-add_stock(x,con,sec,i)

x<-TX
i<-add_stock(x,con,sec,i)

x<-HUN
i<-add_stock(x,con,sec,i)

x<-ROC
i<-add_stock(x,con,sec,i)

x<-ATI
i<-add_stock(x,con,sec,i)

x<-HMY
i<-add_stock(x,con,sec,i)

x<-ATR
i<-add_stock(x,con,sec,i)

x<-NEU
i<-add_stock(x,con,sec,i)

x<-CSL
i<-add_stock(x,con,sec,i)

x<-ANV
i<-add_stock(x,con,sec,i)

x<-OI
i<-add_stock(x,con,sec,i)

x<-PKG
i<-add_stock(x,con,sec,i)

x<-BMS
i<-add_stock(x,con,sec,i)

x<-X
i<-add_stock(x,con,sec,i)

x<-CYT
i<-add_stock(x,con,sec,i)

x<-SON
i<-add_stock(x,con,sec,i)

x<-SEE
i<-add_stock(x,con,sec,i)

x<-MTL
i<-add_stock(x,con,sec,i)

x<-SLGN
i<-add_stock(x,con,sec,i)

x<-PAAS
i<-add_stock(x,con,sec,i)

x<-CRS
i<-add_stock(x,con,sec,i)

x<-MEOH
i<-add_stock(x,con,sec,i)

x<-UFS
i<-add_stock(x,con,sec,i)

x<-SMG
i<-add_stock(x,con,sec,i)

x<-STLD
i<-add_stock(x,con,sec,i)

x<-CBT
i<-add_stock(x,con,sec,i)

x<-CMP
i<-add_stock(x,con,sec,i)

x<-AG
i<-add_stock(x,con,sec,i)

x<-CR
i<-add_stock(x,con,sec,i)

x<-GPK
i<-add_stock(x,con,sec,i)

x<-TIE
i<-add_stock(x,con,sec,i)

x<-CDE
i<-add_stock(x,con,sec,i)

x<-AWC
i<-add_stock(x,con,sec,i)

x<-GEF
i<-add_stock(x,con,sec,i)

x<-ATU
i<-add_stock(x,con,sec,i)

x<-SMS
i<-add_stock(x,con,sec,i)

x<-KRO
i<-add_stock(x,con,sec,i)

x<-SHI
i<-add_stock(x,con,sec,i)

x<-SXT
i<-add_stock(x,con,sec,i)

x<-UAN
i<-add_stock(x,con,sec,i)

x<-OLN
i<-add_stock(x,con,sec,i)

x<-IPI
i<-add_stock(x,con,sec,i)

x<-PDH
i<-add_stock(x,con,sec,i)

x<-MLI
i<-add_stock(x,con,sec,i)

x<-CHMT
i<-add_stock(x,con,sec,i)

x<-PPO
i<-add_stock(x,con,sec,i)

x<-HBM
i<-add_stock(x,con,sec,i)

x<-RXN
i<-add_stock(x,con,sec,i)

x<-CMC
i<-add_stock(x,con,sec,i)

x<-FUL
i<-add_stock(x,con,sec,i)

x<-AUQ
i<-add_stock(x,con,sec,i)

x<-HL
i<-add_stock(x,con,sec,i)

x<-WOR
i<-add_stock(x,con,sec,i)

x<-CW
i<-add_stock(x,con,sec,i)

x<-NG
i<-add_stock(x,con,sec,i)

x<-POL
i<-add_stock(x,con,sec,i)

x<-SWC
i<-add_stock(x,con,sec,i)

x<-RFP
i<-add_stock(x,con,sec,i)

x<-GGC
i<-add_stock(x,con,sec,i)

x<-RNF
i<-add_stock(x,con,sec,i)

x<-MCP
i<-add_stock(x,con,sec,i)

x<-SSD
i<-add_stock(x,con,sec,i)

x<-WTS
i<-add_stock(x,con,sec,i)

x<-MTX
i<-add_stock(x,con,sec,i)

x<-BKI
i<-add_stock(x,con,sec,i)

x<-SSRI
i<-add_stock(x,con,sec,i)

x<-GSM
i<-add_stock(x,con,sec,i)

x<-MUX
i<-add_stock(x,con,sec,i)

x<-KALU
i<-add_stock(x,con,sec,i)

x<-RAVN
i<-add_stock(x,con,sec,i)

x<-HWD
i<-add_stock(x,con,sec,i)

x<-BCPC
i<-add_stock(x,con,sec,i)

x<-IPHS
i<-add_stock(x,con,sec,i)

x<-ROLL
i<-add_stock(x,con,sec,i)

x<-GORO
i<-add_stock(x,con,sec,i)

x<-RBY
i<-add_stock(x,con,sec,i)

x<-SVM
i<-add_stock(x,con,sec,i)

x<-SWM
i<-add_stock(x,con,sec,i)

x<-KS
i<-add_stock(x,con,sec,i)

x<-EXK
i<-add_stock(x,con,sec,i)

x<-KAMN
i<-add_stock(x,con,sec,i)

x<-BAA
i<-add_stock(x,con,sec,i)

x<-CLW
i<-add_stock(x,con,sec,i)

x<-LXU
i<-add_stock(x,con,sec,i)

x<-AVD
i<-add_stock(x,con,sec,i)

x<-NSU
i<-add_stock(x,con,sec,i)

x<-TRS
i<-add_stock(x,con,sec,i)

x<-MATW
i<-add_stock(x,con,sec,i)

x<-CCC
i<-add_stock(x,con,sec,i)

x<-BZ
i<-add_stock(x,con,sec,i)

x<-WDFC
i<-add_stock(x,con,sec,i)

x<-DEL
i<-add_stock(x,con,sec,i)

x<-SCHN
i<-add_stock(x,con,sec,i)

x<-NPO
i<-add_stock(x,con,sec,i)

x<-UFPI
i<-add_stock(x,con,sec,i)

x<-SA
i<-add_stock(x,con,sec,i)

x<-AZK
i<-add_stock(x,con,sec,i)

x<-KRA
i<-add_stock(x,con,sec,i)

x<-KIOR
i<-add_stock(x,con,sec,i)

x<-GLT
i<-add_stock(x,con,sec,i)

x<-SHLM
i<-add_stock(x,con,sec,i)

x<-KOP
i<-add_stock(x,con,sec,i)

x<-KDN
i<-add_stock(x,con,sec,i)

x<-IOSP
i<-add_stock(x,con,sec,i)

x<-RTI
i<-add_stock(x,con,sec,i)

x<-AIN
i<-add_stock(x,con,sec,i)

x<-MWA
i<-add_stock(x,con,sec,i)

x<-CENX
i<-add_stock(x,con,sec,i)

x<-SNHY
i<-add_stock(x,con,sec,i)

x<-HAYN
i<-add_stock(x,con,sec,i)

x<-AKS
i<-add_stock(x,con,sec,i)

x<-TPCG
i<-add_stock(x,con,sec,i)

x<-MVG
i<-add_stock(x,con,sec,i)

x<-CIR
i<-add_stock(x,con,sec,i)

x<-TGB
i<-add_stock(x,con,sec,i)

x<-OMG
i<-add_stock(x,con,sec,i)

x<-EMO
i<-add_stock(x,con,sec,i)

x<-GFF
i<-add_stock(x,con,sec,i)

x<-WIRE
i<-add_stock(x,con,sec,i)

x<-KWR
i<-add_stock(x,con,sec,i)

x<-TC
i<-add_stock(x,con,sec,i)

x<-NL
i<-add_stock(x,con,sec,i)

x<-MUSA
i<-add_stock(x,con,sec,i)

x<-TG
i<-add_stock(x,con,sec,i)

x<-DNN
i<-add_stock(x,con,sec,i)

x<-TREX
i<-add_stock(x,con,sec,i)

x<-MYE
i<-add_stock(x,con,sec,i)

x<-RTK
i<-add_stock(x,con,sec,i)

x<-NEWP
i<-add_stock(x,con,sec,i)

x<-NOR
i<-add_stock(x,con,sec,i)

x<-TRX
i<-add_stock(x,con,sec,i)

x<-NP
i<-add_stock(x,con,sec,i)

x<-WPP
i<-add_stock(x,con,sec,i)

x<-ZINC
i<-add_stock(x,con,sec,i)

x<-FF
i<-add_stock(x,con,sec,i)

x<-HWKN
i<-add_stock(x,con,sec,i)

x<-GSS
i<-add_stock(x,con,sec,i)

x<-OMN
i<-add_stock(x,con,sec,i)

x<-AZC
i<-add_stock(x,con,sec,i)

x<-PZG
i<-add_stock(x,con,sec,i)

x<-PAL
i<-add_stock(x,con,sec,i)

x<-MERC
i<-add_stock(x,con,sec,i)

x<-FSIN
i<-add_stock(x,con,sec,i)

x<-FSTR
i<-add_stock(x,con,sec,i)

x<-NAK
i<-add_stock(x,con,sec,i)

x<-EDG
i<-add_stock(x,con,sec,i)

x<-FOE
i<-add_stock(x,con,sec,i)

x<-PLPC
i<-add_stock(x,con,sec,i)

x<-CAS
i<-add_stock(x,con,sec,i)

x<-GPL
i<-add_stock(x,con,sec,i)

x<-KGN
i<-add_stock(x,con,sec,i)

x<-LNDC
i<-add_stock(x,con,sec,i)

x<-GMO
i<-add_stock(x,con,sec,i)

x<-SVLC
i<-add_stock(x,con,sec,i)

x<-VGZ
i<-add_stock(x,con,sec,i)

x<-USAP
i<-add_stock(x,con,sec,i)

x<-ACET
i<-add_stock(x,con,sec,i)

x<-THM
i<-add_stock(x,con,sec,i)

x<-ZAGG
i<-add_stock(x,con,sec,i)

x<-AUMN
i<-add_stock(x,con,sec,i)

x<-AXU
i<-add_stock(x,con,sec,i)

x<-ADES
i<-add_stock(x,con,sec,i)

x<-ARSD
i<-add_stock(x,con,sec,i)

x<-MCC
i<-add_stock(x,con,sec,i)

x<-DRD
i<-add_stock(x,con,sec,i)

x<-UEC
i<-add_stock(x,con,sec,i)

x<-YONG
i<-add_stock(x,con,sec,i)

x<-BOOM
i<-add_stock(x,con,sec,i)

x<-KMG
i<-add_stock(x,con,sec,i)

x<-GRZ
i<-add_stock(x,con,sec,i)

x<-PLM
i<-add_stock(x,con,sec,i)

x<-PLG
i<-add_stock(x,con,sec,i)

x<-CXDC
i<-add_stock(x,con,sec,i)

x<-REGI
i<-add_stock(x,con,sec,i)

x<-AMRS
i<-add_stock(x,con,sec,i)

x<-AVL
i<-add_stock(x,con,sec,i)

x<-HNH
i<-add_stock(x,con,sec,i)

x<-AP
i<-add_stock(x,con,sec,i)

x<-SHLO
i<-add_stock(x,con,sec,i)

x<-ZEUS
i<-add_stock(x,con,sec,i)

x<-MDW
i<-add_stock(x,con,sec,i)

x<-CCIX
i<-add_stock(x,con,sec,i)

x<-AAU
i<-add_stock(x,con,sec,i)

x<-UAMY
i<-add_stock(x,con,sec,i)

x<-SEH
i<-add_stock(x,con,sec,i)

x<-GSE
i<-add_stock(x,con,sec,i)

x<-GPRE
i<-add_stock(x,con,sec,i)

x<-XRA
i<-add_stock(x,con,sec,i)

x<-CIX
i<-add_stock(x,con,sec,i)

dbDisconnect(con)