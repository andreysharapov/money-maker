data<-zeroyld
tv<-TVECM(data, nthresh=2,lag=1, ngridBeta=20, ngridTh=30, plot=TRUE,trim=0.05, common="All")
c1<-tv$model.specific$coint[1]
c2<-tv$model.specific$coint[2]
series<-c1*data[1:482,1]+c2*data[1:482,2]
m<-mean(series)
v<-sqrt(var(series))
plot(series,type="l")
abline(m,0,col="red")
abline(m+v,0,col="green")
abline(m-v,0,col="green")
abline(m+2*v,0,col="blue")
abline(m-2*v,0,col="blue")

dl<-dim(dataF)[1]
i<-1
j<-8
data<-as.data.frame(t(rbind(t(dataF[1:dl,i]),t(dataF[1:dl,j]))))
plot(data[1:dl,1],type="l")
plot(data[1:dl,2],type="l")
tv<-TVECM(data, nthresh=2,lag=2, ngridBeta=20, ngridTh=30, plot=FALSE,trim=0.03, common="All")
c1<-tv$model.specific$coint[1]
c2<-tv$model.specific$coint[2]
series<-c1*data[1:482,1]+c2*data[1:482,2]
m<-mean(series)
v<-sqrt(var(series))
plot(series,type="l")
abline(m,0,col="red")
abline(m+v,0,col="green")
abline(m-v,0,col="green")
abline(m+2*v,0,col="blue")
abline(m-2*v,0,col="blue")


test2<-TVECM.SeoTest(data,lag=2, beta=1, trim=0.1,nboot=2, plot=FALSE,check=TRUE)
test3<-TVECM.SeoTest(data,lag=2, beta=1, trim=0.1,nboot=1000, plot=FALSE,check=TRUE)
testCT<-TVECM.SeoTest(dataC,lag=2, beta=1, trim=0.1,nboot=10, plot=FALSE,check=TRUE)