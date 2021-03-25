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

for(j in 1:length(stock_list))
{
  getSymbols(stock_list[j],src="yahoo")
}

drv = dbDriver("MySQL")
con = dbConnect(drv,dbname="stockdata",user="root",pass="password")
dbGetQuery(con, "delete from price")
i=1

x<-UTX
i<-add_stock(x,con,sec,i)

x<-MMM
i<-add_stock(x,con,sec,i)

x<-CAT
i<-add_stock(x,con,sec,i)

x<-BA
i<-add_stock(x,con,sec,i)

x<-DE
i<-add_stock(x,con,sec,i)

x<-LMT
i<-add_stock(x,con,sec,i)

x<-TS
i<-add_stock(x,con,sec,i)

x<-GD
i<-add_stock(x,con,sec,i)

x<-RTN
i<-add_stock(x,con,sec,i)

x<-CMI
i<-add_stock(x,con,sec,i)

x<-NOC
i<-add_stock(x,con,sec,i)

x<-CRH
i<-add_stock(x,con,sec,i)

x<-IR
i<-add_stock(x,con,sec,i)

x<-FAST
i<-add_stock(x,con,sec,i)

x<-KUB
i<-add_stock(x,con,sec,i)

x<-CNH
i<-add_stock(x,con,sec,i)

x<-FLR
i<-add_stock(x,con,sec,i)

x<-CX
i<-add_stock(x,con,sec,i)

x<-COL
i<-add_stock(x,con,sec,i)

x<-TDG
i<-add_stock(x,con,sec,i)

x<-LLL
i<-add_stock(x,con,sec,i)

x<-FLS
i<-add_stock(x,con,sec,i)

x<-PLL
i<-add_stock(x,con,sec,i)

x<-DHI
i<-add_stock(x,con,sec,i)

x<-LEN
i<-add_stock(x,con,sec,i)

x<-PHM
i<-add_stock(x,con,sec,i)

x<-VMC
i<-add_stock(x,con,sec,i)

x<-TOL
i<-add_stock(x,con,sec,i)

x<-ST
i<-add_stock(x,con,sec,i)

x<-JEC
i<-add_stock(x,con,sec,i)

x<-DCI
i<-add_stock(x,con,sec,i)

x<-MAS
i<-add_stock(x,con,sec,i)

x<-PWR
i<-add_stock(x,con,sec,i)

x<-ERJ
i<-add_stock(x,con,sec,i)

x<-XYL
i<-add_stock(x,con,sec,i)

x<-MSM
i<-add_stock(x,con,sec,i)

x<-KBR
i<-add_stock(x,con,sec,i)

x<-PNR
i<-add_stock(x,con,sec,i)

x<-AGCO
i<-add_stock(x,con,sec,i)

x<-FBHS
i<-add_stock(x,con,sec,i)

x<-BEAV
i<-add_stock(x,con,sec,i)

x<-DRC
i<-add_stock(x,con,sec,i)

x<-MLM
i<-add_stock(x,con,sec,i)

x<-NDSN
i<-add_stock(x,con,sec,i)

x<-TKR
i<-add_stock(x,con,sec,i)

x<-JHX
i<-add_stock(x,con,sec,i)

x<-CBI
i<-add_stock(x,con,sec,i)

x<-SPR
i<-add_stock(x,con,sec,i)

x<-VMI
i<-add_stock(x,con,sec,i)

x<-LECO
i<-add_stock(x,con,sec,i)

x<-EIX
i<-add_stock(x,con,sec,i)

x<-SPW
i<-add_stock(x,con,sec,i)

x<-CFX
i<-add_stock(x,con,sec,i)

x<-KMT
i<-add_stock(x,con,sec,i)

x<-GGG
i<-add_stock(x,con,sec,i)

x<-FLIR
i<-add_stock(x,con,sec,i)

x<-GDI
i<-add_stock(x,con,sec,i)

x<-RBC
i<-add_stock(x,con,sec,i)

x<-TGI
i<-add_stock(x,con,sec,i)

x<-URS
i<-add_stock(x,con,sec,i)

x<-SHAW
i<-add_stock(x,con,sec,i)

x<-WSO
i<-add_stock(x,con,sec,i)

x<-TEX
i<-add_stock(x,con,sec,i)

x<-AWI
i<-add_stock(x,con,sec,i)

x<-TRN
i<-add_stock(x,con,sec,i)

x<-RBN
i<-add_stock(x,con,sec,i)

x<-USG
i<-add_stock(x,con,sec,i)

x<-VC
i<-add_stock(x,con,sec,i)

x<-WWD
i<-add_stock(x,con,sec,i)

x<-FWLT
i<-add_stock(x,con,sec,i)

x<-TDY
i<-add_stock(x,con,sec,i)

x<-ACM
i<-add_stock(x,con,sec,i)

x<-LII
i<-add_stock(x,con,sec,i)

x<-TTC
i<-add_stock(x,con,sec,i)

x<-MIDD
i<-add_stock(x,con,sec,i)

x<-GTLS
i<-add_stock(x,con,sec,i)

x<-EXP
i<-add_stock(x,con,sec,i)

x<-EME
i<-add_stock(x,con,sec,i)

x<-ZBRA
i<-add_stock(x,con,sec,i)

x<-LPX
i<-add_stock(x,con,sec,i)

x<-SHS
i<-add_stock(x,con,sec,i)

x<-ITT
i<-add_stock(x,con,sec,i)

x<-XLS
i<-add_stock(x,con,sec,i)

x<-HEI
i<-add_stock(x,con,sec,i)

x<-MTW
i<-add_stock(x,con,sec,i)

x<-AIT
i<-add_stock(x,con,sec,i)

x<-ESL
i<-add_stock(x,con,sec,i)

x<-THO
i<-add_stock(x,con,sec,i)

x<-HSC
i<-add_stock(x,con,sec,i)

x<-MDC
i<-add_stock(x,con,sec,i)

x<-GRP
i<-add_stock(x,con,sec,i)

x<-ATK
i<-add_stock(x,con,sec,i)

x<-STN
i<-add_stock(x,con,sec,i)

x<-MTZ
i<-add_stock(x,con,sec,i)

x<-ESLT
i<-add_stock(x,con,sec,i)

x<-BECN
i<-add_stock(x,con,sec,i)

x<-SPF
i<-add_stock(x,con,sec,i)

x<-MTH
i<-add_stock(x,con,sec,i)

x<-B
i<-add_stock(x,con,sec,i)

x<-CUB
i<-add_stock(x,con,sec,i)

x<-RYL
i<-add_stock(x,con,sec,i)

x<-CPAC
i<-add_stock(x,con,sec,i)

x<-ENTG
i<-add_stock(x,con,sec,i)

x<-WAIR
i<-add_stock(x,con,sec,i)

x<-TXI
i<-add_stock(x,con,sec,i)

x<-GVA
i<-add_stock(x,con,sec,i)

x<-ICA
i<-add_stock(x,con,sec,i)

x<-ACO
i<-add_stock(x,con,sec,i)

x<-KBH
i<-add_stock(x,con,sec,i)

x<-GFA
i<-add_stock(x,con,sec,i)

x<-BGG
i<-add_stock(x,con,sec,i)

x<-NC
i<-add_stock(x,con,sec,i)

x<-LNN
i<-add_stock(x,con,sec,i)

x<-ORB
i<-add_stock(x,con,sec,i)

x<-TNC
i<-add_stock(x,con,sec,i)

x<-SODA
i<-add_stock(x,con,sec,i)

x<-SLCA
i<-add_stock(x,con,sec,i)

x<-HXM
i<-add_stock(x,con,sec,i)

x<-ASTE
i<-add_stock(x,con,sec,i)

x<-BLT
i<-add_stock(x,con,sec,i)

x<-AIR
i<-add_stock(x,con,sec,i)

x<-NX
i<-add_stock(x,con,sec,i)

x<-DXPE
i<-add_stock(x,con,sec,i)

x<-DW
i<-add_stock(x,con,sec,i)

x<-PRIM
i<-add_stock(x,con,sec,i)

x<-RSTI
i<-add_stock(x,con,sec,i)

x<-GY
i<-add_stock(x,con,sec,i)

x<-CASC
i<-add_stock(x,con,sec,i)

x<-GRC
i<-add_stock(x,con,sec,i)

x<-FOR
i<-add_stock(x,con,sec,i)

x<-HCLP
i<-add_stock(x,con,sec,i)

x<-SXI
i<-add_stock(x,con,sec,i)

x<-AMRC
i<-add_stock(x,con,sec,i)

x<-TPC
i<-add_stock(x,con,sec,i)

x<-NPK
i<-add_stock(x,con,sec,i)

x<-AVAV
i<-add_stock(x,con,sec,i)

x<-AIMC
i<-add_stock(x,con,sec,i)

x<-DY
i<-add_stock(x,con,sec,i)

x<-CYD
i<-add_stock(x,con,sec,i)

x<-HOV
i<-add_stock(x,con,sec,i)

x<-APOG
i<-add_stock(x,con,sec,i)

x<-JBT
i<-add_stock(x,con,sec,i)

x<-AAON
i<-add_stock(x,con,sec,i)

x<-HW
i<-add_stock(x,con,sec,i)

x<-GLDD
i<-add_stock(x,con,sec,i)

x<-CSTE
i<-add_stock(x,con,sec,i)

x<-MYRG
i<-add_stock(x,con,sec,i)

x<-FIX
i<-add_stock(x,con,sec,i)

x<-TITN
i<-add_stock(x,con,sec,i)

x<-BZH
i<-add_stock(x,con,sec,i)

x<-GIFI
i<-add_stock(x,con,sec,i)

x<-ROCK
i<-add_stock(x,con,sec,i)

x<-ALG
i<-add_stock(x,con,sec,i)

x<-LAYN
i<-add_stock(x,con,sec,i)

x<-MHO
i<-add_stock(x,con,sec,i)

x<-ATRO
i<-add_stock(x,con,sec,i)

x<-WGO
i<-add_stock(x,con,sec,i)

x<-CVCO
i<-add_stock(x,con,sec,i)

x<-GLPW
i<-add_stock(x,con,sec,i)

x<-PIKE
i<-add_stock(x,con,sec,i)

x<-CMCO
i<-add_stock(x,con,sec,i)

x<-AMWD
i<-add_stock(x,con,sec,i)

x<-ANEN
i<-add_stock(x,con,sec,i)

x<-KTOS
i<-add_stock(x,con,sec,i)

x<-KAI
i<-add_stock(x,con,sec,i)

x<-NWPX
i<-add_stock(x,con,sec,i)

x<-LMIA
i<-add_stock(x,con,sec,i)

x<-LDL
i<-add_stock(x,con,sec,i)

x<-BKR
i<-add_stock(x,con,sec,i)

x<-NCS
i<-add_stock(x,con,sec,i)

x<-TWIN
i<-add_stock(x,con,sec,i)

x<-AGX
i<-add_stock(x,con,sec,i)

x<-ORN
i<-add_stock(x,con,sec,i)

x<-XIN
i<-add_stock(x,con,sec,i)

x<-IIIN
i<-add_stock(x,con,sec,i)

x<-GHM
i<-add_stock(x,con,sec,i)

x<-PGTI
i<-add_stock(x,con,sec,i)

x<-STRL
i<-add_stock(x,con,sec,i)

x<-DCO
i<-add_stock(x,con,sec,i)

x<-HURC
i<-add_stock(x,con,sec,i)

x<-PATK
i<-add_stock(x,con,sec,i)

x<-CECE
i<-add_stock(x,con,sec,i)

x<-BXC
i<-add_stock(x,con,sec,i)

x<-TGE
i<-add_stock(x,con,sec,i)

x<-MPR
i<-add_stock(x,con,sec,i)

x<-ERII
i<-add_stock(x,con,sec,i)

x<-FTEK
i<-add_stock(x,con,sec,i)

x<-HDNG
i<-add_stock(x,con,sec,i)

x<-CVU
i<-add_stock(x,con,sec,i)

x<-MNTX
i<-add_stock(x,con,sec,i)

x<-SCX
i<-add_stock(x,con,sec,i)

x<-HDSN
i<-add_stock(x,con,sec,i)

x<-EDAC
i<-add_stock(x,con,sec,i)

dbDisconnect(con)