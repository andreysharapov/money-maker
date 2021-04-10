Return <- function(price.pair, signal.lagged, hedge.ratio.lagged)
{
  #
  signal      <- as.xts(na.omit(cbind(signal.lagged, -1*(signal.lagged))))
  return.pair <- as.xts(na.omit(.return(price.pair, type = "discrete"))) 
  weight.pair <- as.xts(na.omit(HedgeRatio2Weight(hedge.ratio.lagged)))
  #
  #names(return.pair) <- names(price.pair)
  #names(signal)      <- names(price.pair)
  #names(weight.pair) <- names(price.pair) 
  #as.xts(apply(signal * weight.pair * return.pair, 1, sum) * leverage)
  x <-          as.xts(apply(merge(signal[, 1], weight.pair[, 1], return.pair[, 1], all = FALSE), 1, prod))
  x <- merge(x, as.xts(apply(merge(signal[, 2], weight.pair[, 2], return.pair[, 2], all = FALSE), 1, prod)))

  if(!length(dim(x))){
    xts(rep(NA, nrow(price.pair)), order.by = index(price.pair))
  }else{
    xts(rowSums(x), order.by = index(x))
  }
}

ReturnJo <- function(price.pair, signal.lagged, hedge.ratio.lagged) {
  #
  # should be sign(hedge.ratio.lagged) instead of -1 because some pairs require tp buy both assets
  signal      <- as.xts(na.omit(cbind(signal.lagged, signal.lagged, sign(hedge.ratio.lagged))))
  signal[,2]  <- signal[,2]*signal[,3]
  signal      <- signal[,1:2]
  return.pair <- as.xts(na.omit(.return(price.pair, type = "discrete"))) 
  weight.pair <- as.xts(na.omit(HedgeRatio2WeightJo(hedge.ratio.lagged)))
  #
  #names(return.pair) <- names(price.pair)
  #names(signal)      <- names(price.pair)
  #names(weight.pair) <- names(price.pair) 
  #as.xts(apply(signal * weight.pair * return.pair, 1, sum) * leverage)
  x <-          as.xts(apply(merge(signal[, 1], weight.pair[, 1], return.pair[, 1], all = FALSE), 1, prod))
  x <- merge(x, as.xts(apply(merge(signal[, 2], weight.pair[, 2], return.pair[, 2], all = FALSE), 1, prod)))
  
  if(!length(dim(x))){
    xts(rep(NA, nrow(price.pair)), order.by = index(price.pair))
  }else{
    xts(rowSums(x), order.by = index(x))
  }
}

ReturnTripleJo <- function(price.triple, signal.lagged, hedge.ratio.lagged_1, hedge.ratio.lagged_2) {
  #
  # should be sign(hedge.ratio.lagged) instead of -1 because some pairs require tp buy both assets
  signal      <- as.xts(na.omit(cbind(signal.lagged, signal.lagged, signal.lagged, sign(hedge.ratio.lagged_1), sign(hedge.ratio.lagged_2))))
  signal[,2]  <- signal[,2]*signal[,4]
  signal[,3]  <- signal[,3]*signal[,5]
  signal      <- signal[,1:3]
  return.triple <- as.xts(na.omit(.return(price.triple, type = "discrete"))) 
  weight.triple <- as.xts(na.omit(HedgeRatio2WeightJoTriple(hedge.ratio.lagged_1, hedge.ratio.lagged_2)))
  #
  #names(return.pair) <- names(price.pair)
  #names(signal)      <- names(price.pair)
  #names(weight.pair) <- names(price.pair) 
  #as.xts(apply(signal * weight.pair * return.pair, 1, sum) * leverage)
  x <-          as.xts(apply(merge(signal[, 1], weight.triple[, 1], return.triple[, 1], all = FALSE), 1, prod))
  x <- merge(x, as.xts(apply(merge(signal[, 2], weight.triple[, 2], return.triple[, 2], all = FALSE), 1, prod)))
  x <- merge(x, as.xts(apply(merge(signal[, 3], weight.triple[, 3], return.triple[, 3], all = FALSE), 1, prod)))
  
  if(!length(dim(x))){
    xts(rep(NA, nrow(price.triple)), order.by = index(price.triple))
  }else{
    xts(rowSums(x), order.by = index(x))
  }
}
.return <- function(x, type = c("continuous", "discrete"), na.pad = TRUE) 
{
    type <- match.arg(type)
    if (type == "discrete") {
        result <- x/lag(x, na.pad = na.pad) - 1
    }else if (type == "continuous") {
        result <- diff(log(x), na.pad = na.pad)
    }
    return(result)
}