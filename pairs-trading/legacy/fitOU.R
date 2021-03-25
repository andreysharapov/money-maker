fitOU <- function(S){
  n = length(S)-1;
  end=length(S)
  delta=1           
  Sx  = sum( S[1:end-1] );
  Sy  = sum( S[2:end] );
  Sxx = sum( S[1:end-1]^2 );
  Sxy = sum( S[1:end-1]*S[2:end] );
  Syy = sum( S[2:end]^2 );
  
  a  = ( n*Sxy - Sx*Sy ) / ( n*Sxx -Sx^2 );
  b  = ( Sy - a*Sx ) / n;
  sd = sqrt( (n*Syy - Sy^2 - a*(n*Sxy - Sx*Sy) )/n/(n-2) );
  
  lambda = -log(a)/delta;
  mu     = b/(1-a);
  sigma  =  sd * sqrt( -2*log(a)/delta/(1-a^2) );
  return (c(lambda,mu,sigma))
}

i<-which(stock_pickedCG=="ACO")
j<-which(stock_pickedCG=="AIMC")
datOU<-(-dataCG[1:dl,i]+0.6990608*dataCG[1:dl,j])
param<-fitOU(datOU)
halflife<-log(2)/param[1]
print(halflife)