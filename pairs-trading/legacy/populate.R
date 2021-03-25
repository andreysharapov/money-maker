add_stock<-function(x,con,sec,i,range){
  open<-coredata(Op(x)[range])
  close<-coredata(Cl(x)[range])
  high<-coredata(Hi(x)[range])
  low<-coredata(Lo(x)[range])
  volume<-coredata(Vo(x)[range])
  date<-as.data.frame(as.Date(index(Op(x)[range])))
  ticker<-rep(stock_list[i],length(open))
  sector<-rep(sec,length(open))
  data<-as.data.frame(cbind(date,open,high,low,close,volume,ticker,sector))
  colnames(data)<-c("date","open","high","low","close","volume","ticker","sector")
  dbWriteTable(con, "price", as.data.frame(data),append=T,row.names=F)
  return (i+1)
}
start="2010-09-19"
dataspan <- paste(start,"::",sep ="")
sec="F"
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
for(j in 1:length(stock_list))
{
  getSymbols(stock_list[j],src="yahoo")
}

drv = dbDriver("MySQL")
con = dbConnect(drv,dbname="stockdata",user="root",pass="password")
dbGetQuery(con, "delete from price")
i=1

x<-GE
i<-add_stock(x,con,sec,i,dataspan)

x<-WFC
i<-add_stock(x,con,sec,i,dataspan)

x<-HBC
i<-add_stock(x,con,sec,i,dataspan)

x<-JPM
i<-add_stock(x,con,sec,i,dataspan)

x<-C
i<-add_stock(x,con,sec,i,dataspan)

x<-BAC
i<-add_stock(x,con,sec,i,dataspan)

x<-RY
i<-add_stock(x,con,sec,i,dataspan)

x<-LFC
i<-add_stock(x,con,sec,i,dataspan)

x<-TD
i<-add_stock(x,con,sec,i,dataspan)
  
x<-WBK
i<-add_stock(x,con,sec,i,dataspan)

x<-ITUB
i<-add_stock(x,con,sec,i,dataspan)

x<-SAN
i<-add_stock(x,con,sec,i,dataspan)

x<-AXP
i<-add_stock(x,con,sec,i,dataspan)

x<-BBD
i<-add_stock(x,con,sec,i,dataspan)

x<-USB
i<-add_stock(x,con,sec,i,dataspan)

x<-BNS
i<-add_stock(x,con,sec,i,dataspan)

x<-AVF
i<-add_stock(x,con,sec,i,dataspan)

x<-UNH
i<-add_stock(x,con,sec,i,dataspan)

x<-GS
i<-add_stock(x,con,sec,i,dataspan)

x<-MA
i<-add_stock(x,con,sec,i,dataspan)

x<-SMFG
i<-add_stock(x,con,sec,i,dataspan)

x<-BBVA
i<-add_stock(x,con,sec,i,dataspan)

x<-LYG
i<-add_stock(x,con,sec,i,dataspan)

x<-BCS
i<-add_stock(x,con,sec,i,dataspan)

x<-BLK
i<-add_stock(x,con,sec,i,dataspan)

x<-BMO
i<-add_stock(x,con,sec,i,dataspan)

x<-MET
i<-add_stock(x,con,sec,i,dataspan)

x<-DB
i<-add_stock(x,con,sec,i,dataspan)

x<-PNC
i<-add_stock(x,con,sec,i,dataspan)

x<-PUK
i<-add_stock(x,con,sec,i,dataspan)

x<-COF
i<-add_stock(x,con,sec,i,dataspan)

x<-MS
i<-add_stock(x,con,sec,i,dataspan)

x<-ING
i<-add_stock(x,con,sec,i,dataspan)

x<-CM
i<-add_stock(x,con,sec,i,dataspan)

x<-AFG
i<-add_stock(x,con,sec,i,dataspan)

x<-CS
i<-add_stock(x,con,sec,i,dataspan)

x<-BK
i<-add_stock(x,con,sec,i,dataspan)

x<-PRU
i<-add_stock(x,con,sec,i,dataspan)

x<-BEN
i<-add_stock(x,con,sec,i,dataspan)

x<-ACE
i<-add_stock(x,con,sec,i,dataspan)

x<-TRV
i<-add_stock(x,con,sec,i,dataspan)

x<-BBT
i<-add_stock(x,con,sec,i,dataspan)

x<-AFL
i<-add_stock(x,con,sec,i,dataspan)

x<-MFC
i<-add_stock(x,con,sec,i,dataspan)

x<-STT
i<-add_stock(x,con,sec,i,dataspan)

x<-CB
i<-add_stock(x,con,sec,i,dataspan)

x<-IBN
i<-add_stock(x,con,sec,i,dataspan)

x<-WLP
i<-add_stock(x,con,sec,i,dataspan)

x<-DFS
i<-add_stock(x,con,sec,i,dataspan)

x<-CME
i<-add_stock(x,con,sec,i,dataspan)

x<-MMC
i<-add_stock(x,con,sec,i,dataspan)

x<-ALL
i<-add_stock(x,con,sec,i,dataspan)

x<-SCHW
i<-add_stock(x,con,sec,i,dataspan)

x<-BX
i<-add_stock(x,con,sec,i,dataspan)

x<-NLY
i<-add_stock(x,con,sec,i,dataspan)

x<-AON
i<-add_stock(x,con,sec,i,dataspan)

x<-AV
i<-add_stock(x,con,sec,i,dataspan)

x<-L
i<-add_stock(x,con,sec,i,dataspan)

x<-TROW
i<-add_stock(x,con,sec,i,dataspan)

x<-SHG
i<-add_stock(x,con,sec,i,dataspan)

x<-STI
i<-add_stock(x,con,sec,i,dataspan)

x<-FITB
i<-add_stock(x,con,sec,i,dataspan)

x<-CI
i<-add_stock(x,con,sec,i,dataspan)

x<-NMR
i<-add_stock(x,con,sec,i,dataspan)

x<-KB
i<-add_stock(x,con,sec,i,dataspan)

x<-AET
i<-add_stock(x,con,sec,i,dataspan)

x<-CIB
i<-add_stock(x,con,sec,i,dataspan)

x<-BCH
i<-add_stock(x,con,sec,i,dataspan)

x<-PGR
i<-add_stock(x,con,sec,i,dataspan)

x<-AMP
i<-add_stock(x,con,sec,i,dataspan)

x<-HUM
i<-add_stock(x,con,sec,i,dataspan)

x<-MTB
i<-add_stock(x,con,sec,i,dataspan)

x<-NTRS
i<-add_stock(x,con,sec,i,dataspan)

x<-IVZ
i<-add_stock(x,con,sec,i,dataspan)

x<-RF
i<-add_stock(x,con,sec,i,dataspan)

x<-AEG
i<-add_stock(x,con,sec,i,dataspan)

x<-ICE
i<-add_stock(x,con,sec,i,dataspan)

x<-BAP
i<-add_stock(x,con,sec,i,dataspan)

x<-FIS
i<-add_stock(x,con,sec,i,dataspan)

x<-AMTD
i<-add_stock(x,con,sec,i,dataspan)

x<-HIG
i<-add_stock(x,con,sec,i,dataspan)

x<-KEY
i<-add_stock(x,con,sec,i,dataspan)

x<-CIT
i<-add_stock(x,con,sec,i,dataspan)

x<-SLM
i<-add_stock(x,con,sec,i,dataspan)

x<-CNA
i<-add_stock(x,con,sec,i,dataspan)

x<-XL
i<-add_stock(x,con,sec,i,dataspan)

x<-OAK
i<-add_stock(x,con,sec,i,dataspan)

x<-LNC
i<-add_stock(x,con,sec,i,dataspan)

x<-NYX
i<-add_stock(x,con,sec,i,dataspan)

x<-WSH
i<-add_stock(x,con,sec,i,dataspan)

x<-CINF
i<-add_stock(x,con,sec,i,dataspan)

x<-AMG
i<-add_stock(x,con,sec,i,dataspan)

x<-CMA
i<-add_stock(x,con,sec,i,dataspan)

x<-NYB
i<-add_stock(x,con,sec,i,dataspan)

x<-CEF
i<-add_stock(x,con,sec,i,dataspan)

x<-HBAN
i<-add_stock(x,con,sec,i,dataspan)

x<-UNM
i<-add_stock(x,con,sec,i,dataspan)

x<-RE
i<-add_stock(x,con,sec,i,dataspan)

x<-CVH
i<-add_stock(x,con,sec,i,dataspan)

x<-ACGL
i<-add_stock(x,con,sec,i,dataspan)

x<-WRB
i<-add_stock(x,con,sec,i,dataspan)

x<-RJF
i<-add_stock(x,con,sec,i,dataspan)

x<-TMK
i<-add_stock(x,con,sec,i,dataspan)

x<-FNF
i<-add_stock(x,con,sec,i,dataspan)

x<-MSCI
i<-add_stock(x,con,sec,i,dataspan)

x<-RGA
i<-add_stock(x,con,sec,i,dataspan)

x<-AJG
i<-add_stock(x,con,sec,i,dataspan)

x<-FRC
i<-add_stock(x,con,sec,i,dataspan)

x<-PBCT
i<-add_stock(x,con,sec,i,dataspan)

x<-NDAQ
i<-add_stock(x,con,sec,i,dataspan)

x<-BOKF
i<-add_stock(x,con,sec,i,dataspan)

x<-RNR
i<-add_stock(x,con,sec,i,dataspan)

x<-ARCC
i<-add_stock(x,con,sec,i,dataspan)

x<-HCBK
i<-add_stock(x,con,sec,i,dataspan)

x<-SEIC
i<-add_stock(x,con,sec,i,dataspan)

x<-CBOE
i<-add_stock(x,con,sec,i,dataspan)

x<-ZION
i<-add_stock(x,con,sec,i,dataspan)

x<-ACAS
i<-add_stock(x,con,sec,i,dataspan)

x<-LAZ
i<-add_stock(x,con,sec,i,dataspan)

x<-LM
i<-add_stock(x,con,sec,i,dataspan)

x<-OZM
i<-add_stock(x,con,sec,i,dataspan)

x<-CFR
i<-add_stock(x,con,sec,i,dataspan)

x<-OCN
i<-add_stock(x,con,sec,i,dataspan)

x<-KKR
i<-add_stock(x,con,sec,i,dataspan)

x<-HCC
i<-add_stock(x,con,sec,i,dataspan)

x<-GPN
i<-add_stock(x,con,sec,i,dataspan)

x<-EV
i<-add_stock(x,con,sec,i,dataspan)

x<-LPLA
i<-add_stock(x,con,sec,i,dataspan)

x<-VR
i<-add_stock(x,con,sec,i,dataspan)

x<-JEF
i<-add_stock(x,con,sec,i,dataspan)

x<-EWBC
i<-add_stock(x,con,sec,i,dataspan)

x<-SBNY
i<-add_stock(x,con,sec,i,dataspan)

x<-BR
i<-add_stock(x,con,sec,i,dataspan)

x<-AIZ
i<-add_stock(x,con,sec,i,dataspan)

x<-AGO
i<-add_stock(x,con,sec,i,dataspan)

x<-FNFG
i<-add_stock(x,con,sec,i,dataspan)

x<-CYN
i<-add_stock(x,con,sec,i,dataspan)

x<-GNW
i<-add_stock(x,con,sec,i,dataspan)

x<-AWH
i<-add_stock(x,con,sec,i,dataspan)

x<-PRA
i<-add_stock(x,con,sec,i,dataspan)

x<-WDR
i<-add_stock(x,con,sec,i,dataspan)

x<-SIVB
i<-add_stock(x,con,sec,i,dataspan)

x<-HBHC
i<-add_stock(x,con,sec,i,dataspan)

x<-KYN
i<-add_stock(x,con,sec,i,dataspan)

x<-NSM
i<-add_stock(x,con,sec,i,dataspan)

x<-ETFC
i<-add_stock(x,con,sec,i,dataspan)

x<-PB
i<-add_stock(x,con,sec,i,dataspan)

x<-DNP
i<-add_stock(x,con,sec,i,dataspan)

x<-ORI
i<-add_stock(x,con,sec,i,dataspan)

x<-FHN
i<-add_stock(x,con,sec,i,dataspan)

x<-BKU
i<-add_stock(x,con,sec,i,dataspan)

x<-ALTE
i<-add_stock(x,con,sec,i,dataspan)

x<-PL
i<-add_stock(x,con,sec,i,dataspan)

x<-CNO
i<-add_stock(x,con,sec,i,dataspan)

x<-ASBC
i<-add_stock(x,con,sec,i,dataspan)

x<-CACC
i<-add_stock(x,con,sec,i,dataspan)

x<-MBI
i<-add_stock(x,con,sec,i,dataspan)

x<-FII
i<-add_stock(x,con,sec,i,dataspan)

x<-BOH
i<-add_stock(x,con,sec,i,dataspan)

x<-MCY
i<-add_stock(x,con,sec,i,dataspan)

x<-FIG
i<-add_stock(x,con,sec,i,dataspan)

x<-FAF
i<-add_stock(x,con,sec,i,dataspan)

x<-AHL
i<-add_stock(x,con,sec,i,dataspan)

x<-ACG
i<-add_stock(x,con,sec,i,dataspan)

x<-ASPS
i<-add_stock(x,con,sec,i,dataspan)

x<-VLY
i<-add_stock(x,con,sec,i,dataspan)

x<-FULT
i<-add_stock(x,con,sec,i,dataspan)

x<-FAX
i<-add_stock(x,con,sec,i,dataspan)

x<-UMBF
i<-add_stock(x,con,sec,i,dataspan)

x<-SUSQ
i<-add_stock(x,con,sec,i,dataspan)

x<-WBS
i<-add_stock(x,con,sec,i,dataspan)

x<-PSEC
i<-add_stock(x,con,sec,i,dataspan)

x<-HNT
i<-add_stock(x,con,sec,i,dataspan)

x<-TCB
i<-add_stock(x,con,sec,i,dataspan)

x<-FMER
i<-add_stock(x,con,sec,i,dataspan)

x<-CFFN
i<-add_stock(x,con,sec,i,dataspan)

x<-SF
i<-add_stock(x,con,sec,i,dataspan)

x<-AFSI
i<-add_stock(x,con,sec,i,dataspan)

x<-APO
i<-add_stock(x,con,sec,i,dataspan)

x<-SNV
i<-add_stock(x,con,sec,i,dataspan)

x<-TCBI
i<-add_stock(x,con,sec,i,dataspan)

x<-PRI
i<-add_stock(x,con,sec,i,dataspan)

x<-KFN
i<-add_stock(x,con,sec,i,dataspan)

x<-WAFD
i<-add_stock(x,con,sec,i,dataspan)

x<-BPOP
i<-add_stock(x,con,sec,i,dataspan)

x<-RATE
i<-add_stock(x,con,sec,i,dataspan)

x<-JNS
i<-add_stock(x,con,sec,i,dataspan)

x<-CSE
i<-add_stock(x,con,sec,i,dataspan)

x<-THG
i<-add_stock(x,con,sec,i,dataspan)

x<-AINV
i<-add_stock(x,con,sec,i,dataspan)

x<-TRMK
i<-add_stock(x,con,sec,i,dataspan)

x<-FNB
i<-add_stock(x,con,sec,i,dataspan)

x<-CEM
i<-add_stock(x,con,sec,i,dataspan)

x<-SYA
i<-add_stock(x,con,sec,i,dataspan)

x<-UMPQ
i<-add_stock(x,con,sec,i,dataspan)

x<-CNS
i<-add_stock(x,con,sec,i,dataspan)

x<-BXS
i<-add_stock(x,con,sec,i,dataspan)

x<-GHL
i<-add_stock(x,con,sec,i,dataspan)

x<-NPBC
i<-add_stock(x,con,sec,i,dataspan)

x<-PTP
i<-add_stock(x,con,sec,i,dataspan)

x<-SFG
i<-add_stock(x,con,sec,i,dataspan)

x<-CATY
i<-add_stock(x,con,sec,i,dataspan)

x<-WTFC
i<-add_stock(x,con,sec,i,dataspan)

dbDisconnect(con)