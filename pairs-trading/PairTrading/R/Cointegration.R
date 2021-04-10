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

EstimateParametersJo <- function(price.pair, confidence, type="eigen", ecdet="none", K=2, spec="longrun") {
  x <- price.pair
  
  test_stat_col <- 3
  if(confidence ==1) {
    test_stat_col <- 3
  } else if(confidence ==5) {
    test_stat_col <- 2
  } else if(confidence ==10) {
    test_stat_col <- 1
  } else {
    throw("Unknown confidence. Only 1,5,10 are allowed")
  }
  
  johtest <-ca.jo(x,type=type,ecdet=ecdet,K=K,spec=spec)
  failed <- 1
  test_r1 = (johtest@teststat[1] < johtest@cval[1,test_stat_col])
  test_r0 = (johtest@teststat[2] > johtest@cval[2,test_stat_col]) 
  if(test_r0 && test_r1) {
    failed <- 0
  }
  hedge.ratio <- as.numeric(johtest@V[2,1])
  premium     <- 0
  spread <- x[,1]+johtest@V[2,1]*x[,2]

  list(spread = spread, hedge.ratio = hedge.ratio, premium = premium, failed = failed)
}

EstimateParametersTripleJo <- function(price.triple, confidence, type="eigen", ecdet="none", K=2, spec="longrun") {
  x <- price.triple
  
  test_stat_col <- 3
  if(confidence ==1) {
    test_stat_col <- 3
  } else if(confidence ==5) {
    test_stat_col <- 2
  } else if(confidence ==10) {
    test_stat_col <- 1
  } else {
    throw("Unknown confidence. Only 1,5,10 are allowed")
  }
  
  johtest <-ca.jo(x,type=type,ecdet=ecdet,K=K,spec=spec)
  failed <- 1
  test_r2 = (johtest@teststat[1] < johtest@cval[1,test_stat_col])
  test_r1 = (johtest@teststat[2] < johtest@cval[2,test_stat_col])
  test_r0 = (johtest@teststat[3] > johtest@cval[3,test_stat_col]) 
  if(test_r0 && test_r1 && test_r2) {
    failed <- 0
  }
  hedge.ratio_1 <- as.numeric(johtest@V[2,1])
  hedge.ratio_2 <- as.numeric(johtest@V[3,1])
  premium     <- 0
  spread <- x[,1] + johtest@V[2,1]*x[,2] + johtest@V[3,1]*x[,3]
  
  list(spread = spread, hedge.ratio_1 = hedge.ratio_1, hedge.ratio_2 = hedge.ratio_2, premium = premium, failed = failed)
}

EstimateParametersHistorically <- function(price.pair, period, method = lm)
{
  Applied <- function(price.pair){
    reg <- EstimateParameters(price.pair, method)
    c(spread = as.numeric(last(reg$spread)), hedge.ratio = reg$hedge.ratio, premium = reg$premium)
  }
  as.xts(rollapplyr(price.pair, period, Applied, by.column = FALSE))
}

EstimateParametersHistoricallyJo <- function(price.pair, period, confidence) {
  Applied <- function(price.pair){
    reg <- EstimateParametersJo(price.pair, confidence)
    c(spread = as.numeric(last(reg$spread)), hedge.ratio = reg$hedge.ratio, premium = reg$premium, 
      m = mean(reg$spread, na.rm=TRUE), stddev = sd(reg$spread, na.rm=TRUE), failed = reg$failed)
  }
  as.xts(rollapplyr(price.pair, period, Applied, by.column = FALSE))
}

EstimateParametersHistoricallyTripleJo <- function(price.triple, period, confidence) {
  Applied <- function(price.triple){
    reg <- EstimateParametersTripleJo(price.triple, confidence)
    c(spread = as.numeric(last(reg$spread)), hedge.ratio_1 = reg$hedge.ratio_1, hedge.ratio_2 = reg$hedge.ratio_2, premium = reg$premium, 
      m = mean(reg$spread, na.rm=TRUE), stddev = sd(reg$spread, na.rm=TRUE), failed = reg$failed)
  }
  as.xts(rollapplyr(price.triple, period, Applied, by.column = FALSE))
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

HedgeRatio2WeightJo <- function(hedge.ratio)
{
  hedge.ratio <- abs(hedge.ratio) * sign(hedge.ratio)
  #
  normalization.factor <- 1 / (1 + abs(hedge.ratio))
  return(cbind(1 * normalization.factor, hedge.ratio * normalization.factor))
}

HedgeRatio2WeightJoTriple <- function(hedge.ratio_1, hedge.ratio_2)
{
  hedge.ratio_1 <- abs(hedge.ratio_1) * sign(hedge.ratio_1)
  hedge.ratio_2 <- abs(hedge.ratio_2) * sign(hedge.ratio_2)
  #
  normalization.factor <- 1 / (1 + abs(hedge.ratio_1) + abs(hedge.ratio_2))
  return(cbind(1 * normalization.factor, hedge.ratio_1 * normalization.factor, hedge.ratio_2 * normalization.factor))
}
