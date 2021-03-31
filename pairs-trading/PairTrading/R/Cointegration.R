#Calculate the spread between two stock prices.
#Assume that log(price) is random walk
#Assume that prices has two column as matrix
EstimateParameters <- function(price.pair, method = lm)
{
  x <- log(price.pair)
  
  reg <- method(x[, 2] ~ x[, 1])
  hedge.ratio <- as.numeric(reg$coef[2])
  premium     <- as.numeric(reg$coef[1])
  spread      <- x[, 2] - (hedge.ratio * x[, 1] + premium)
  list(spread = spread, hedge.ratio = hedge.ratio, premium = premium)
}

EstimateParametersJo <- function(price.pair, type="eigen", ecdet="none", K=2, spec="longrun")
{
  x <- price.pair
  
  johtest <-ca.jo(x,type=type,ecdet=ecdet,K=K,spec=spec)
  failed <- 1
  test_r1 = (johtest@teststat[1] < johtest@cval[1,3])
  test_r0 = (johtest@teststat[2] > johtest@cval[2,3]) 
  if(test_r0 && test_r1) {
    failed <- 0
  }
  hedge.ratio <- as.numeric(johtest@V[2,1])
  premium     <- 0
  spread <- x[,1]+johtest@V[2,1]*x[,2]

  list(spread = spread, hedge.ratio = hedge.ratio, premium = premium, failed = failed)
}

EstimateParametersHistorically <- function(price.pair, period, method = lm)
{
  Applied <- function(price.pair){
    reg <- EstimateParameters(price.pair, method)
    c(spread = as.numeric(last(reg$spread)), hedge.ratio = reg$hedge.ratio, premium = reg$premium)
  }
  as.xts(rollapplyr(price.pair, period, Applied, by.column = FALSE))
}

EstimateParametersHistoricallyJo <- function(price.pair, period)
{
  Applied <- function(price.pair){
    reg <- EstimateParametersJo(price.pair)
    c(spread = as.numeric(last(reg$spread)), hedge.ratio = reg$hedge.ratio, premium = reg$premium, stddev = sd(reg$spread), failed = reg$failed)
  }
  as.xts(rollapplyr(price.pair, period, Applied, by.column = FALSE))
}

#Return wether spread is stationary or not
IsStationary <- function(spread, threshold)
{
  Is.passed.PP.test  <- PP.test(as.numeric(spread))$p.value <= threshold
  Is.passed.adf.test <- adf.test(as.numeric(spread))$p.value <= threshold
  c(PP.test = Is.passed.PP.test, adf.test = Is.passed.adf.test)
}
HedgeRatio2Weight <- function(hedge.ratio)
{
  hedge.ratio <- abs(hedge.ratio) * (-1)
  #
  normalization.factor <- 1 / (1 + abs(hedge.ratio))
  return(cbind(1 * normalization.factor, hedge.ratio * normalization.factor))
}
