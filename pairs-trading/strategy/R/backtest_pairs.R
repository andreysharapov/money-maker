backtest_pairs_jo <- function(pair, ordering, period = 360) {
  
  price.pair <- xts(df[, c(pair$stock_1, pair$stock_2)], order.by = ordering)
  reg <- EstimateParametersJo(price.pair)
  
  params <- EstimateParametersHistoricallyJo(price.pair, period = period)
  num_failures <- sum(params$failed, na.rm = TRUE)
  total_tests <- dim(params)[1] - period + 1
  signal <- SimpleJo(params$spread, params$stddev)
  
  return.pairtrading <- Return(price.pair, lag(signal), lag(params$hedge.ratio))
  cum_return <- 100 * cumprod(1 + return.pairtrading)
  return(list(cum_return = cum_return, lcr = last(cum_return),  percent_failed = 100.0*num_failures/total_tests))
}

backtest_pairs <- function(pair, ordering, period = 360) {
  
  price.pair <- xts(df[, c(pair$stock_1, pair$stock_2)], order.by = ordering)
  reg <- EstimateParameters(price.pair)
  
  params <- EstimateParametersHistorically(price.pair, period = period)
  signal <- Simple(params$spread, 0.05)
  
  return.pairtrading <- Return(price.pair, lag(signal), lag(params$hedge.ratio))
  cum_return <- 100 * cumprod(1 + return.pairtrading)
  return(list(cum_return = cum_return, lcr = last(cum_return)))
}

backtest <- function(pairs, order.by, period) {
  
  sel_pairs_return <- list()
  counter <- 1
  for(pair in pairs) {
    result <- tryCatch({
      jo_returns <- backtest_pairs_jo(pair = pair, ordering = order.by, period = period)
      reg_returns <- backtest_pairs(pair = pair, ordering = order.by, period = period)
      list(jo_returns = jo_returns, reg_returns = reg_returns)
    }, error = function(x) {
      return(NA)
    })
    if(!is.na(result)) {
      pair$jo_returns <- result$jo_returns
      pair$reg_returns <- result$reg_returns
      sel_pairs_return[[counter]] <- pair
      counter <- counter + 1  
    }
  }
  return(sel_pairs_return)
}

prepare_backtest_result <- function(pairs, min_r=110, max_r=300, min_corr=0.7) {
  
  counter <- 1
  pair_returns <- list()
  for(pair in sel_pairs_return) {
    pair_returns[[counter]] <- as.numeric(pair$jo_returns$lcr)
    counter <- counter + 1
  }
  pair_returns <- unlist(pair_returns, use.names = FALSE)
  
  counter <- 1
  perc_failed <- list()
  for(pair in sel_pairs_return) {
    perc_failed[[counter]] <- as.numeric(pair$jo_returns$percent_failed)
    counter <- counter + 1
  }
  perc_failed <- unlist(perc_failed, use.names = FALSE)
  
  counter <- 1
  correl <- list()
  for(pair in sel_pairs_return) {
    correl[[counter]] <- as.numeric(pair$correl[1,2])
    counter <- counter + 1
  }
  correl <- unlist(correl, use.names = FALSE)
  
  returns_failed <- data.frame(pair_returns = pair_returns, perc_failed = perc_failed, correl = correl)
  
  return(returns_failed[((returns_failed$pair_returns > min_r) & (abs(returns_failed$correl) > min_corr) & (returns_failed$pair_returns < max_r)),])
  
}