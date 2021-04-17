backtest_pairs_jo <- function(pair, ordering, confidence, period = 360) {
  
  price.pair <- xts(df[, c(pair$stock_1, pair$stock_2)], order.by = ordering)
  reg <- EstimateParametersJo(price.pair, confidence)
  
  params <- EstimateParametersHistoricallyJo(price.pair, period = period, confidence = confidence)
  num_failures <- sum(params$failed, na.rm = TRUE)
  total_tests <- dim(params)[1] - period + 1
  signal <- SimpleJo(params$spread, params$stddev, params$m)
  
  return.pairtrading <- ReturnJo(price.pair, lag(signal), lag(params$hedge.ratio))
  cum_return <- 100 * cumprod(1 + return.pairtrading)
  return(list(cum_return = cum_return, lcr = last(cum_return),  percent_failed = 100.0*num_failures/total_tests))
}

backtest_triples_jo <- function(pair, ordering, confidence, period = 360) {
  
  price.triple <- xts(df[, c(pair$stock_1, pair$stock_2, pair$stock_3)], order.by = ordering)
  reg <- EstimateParametersTripleJo(price.triple, confidence)
  
  params <- EstimateParametersHistoricallyTripleJo(price.triple, period = period, confidence = confidence)
  num_failures <- sum(params$failed, na.rm = TRUE)
  total_tests <- dim(params)[1] - period + 1
  signal <- SimpleJo(params$spread, params$stddev, params$m)
  
  return.tripletrading <- ReturnTripleJo(price.triple, lag(signal), lag(params$hedge.ratio_1), lag(params$hedge.ratio_2))
  cum_return <- 100 * cumprod(1 + return.tripletrading)
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

backtest <- function(pairs, order.by, period, confidence, backtest_type) {

  if(backtest_type == "pair") {
    sel_pairs_return <- list()
    counter <- 1
    for(pair in pairs) {
      result <- tryCatch({
        jo_returns <- backtest_pairs_jo(pair = pair, ordering = order.by, period = period, confidence = confidence)
        #reg_returns <- backtest_pairs(pair = pair, ordering = order.by, period = period)
        list(jo_returns = jo_returns)
      }, error = function(x) {
        return(NULL)
      })
      if(!is.null(result)) {
        pair$jo_returns <- result$jo_returns
        pair$reg_returns <- NULL
        sel_pairs_return[[counter]] <- pair
        counter <- counter + 1  
      }
    }
    return(sel_pairs_return)    
  } else if(backtest_type == "triple") {
    sel_pairs_return <- list()
    counter <- 1
    for(pair in pairs) {
      result <- tryCatch({
        jo_returns <- backtest_triples_jo(pair = pair, ordering = order.by, period = period, confidence = confidence)
        #reg_returns <- backtest_pairs(pair = pair, ordering = order.by, period = period)
        list(jo_returns = jo_returns)
      }, error = function(x) {
        return(NULL)
      })
      if(!is.null(result)) {
        pair$jo_returns <- result$jo_returns
        pair$reg_returns <- NULL
        sel_pairs_return[[counter]] <- pair
        counter <- counter + 1  
      }
    }
    return(sel_pairs_return)
  } else {
    throw("Unknown backtest type")
  }

}


prepare_backtest_result <- function(pairs, min_r=110, max_r=300, min_profit = 100, failed = 20, min_hl = 7, max_hl = 14) {
  
  counter <- 1
  pair_returns <- list()
  for(pair in pairs) {
    pair_returns[[counter]] <- as.numeric(pair$jo_returns$lcr)
    counter <- counter + 1
  }
  pair_returns <- unlist(pair_returns, use.names = FALSE)
  
  counter <- 1
  perc_failed <- list()
  for(pair in pairs) {
    perc_failed[[counter]] <- as.numeric(pair$jo_returns$percent_failed)
    counter <- counter + 1
  }
  perc_failed <- unlist(perc_failed, use.names = FALSE)
  
  counter <- 1
  correl <- list()
  for(pair in pairs) {
    correl[[counter]] <- as.numeric(pair$correl[1,2])
    counter <- counter + 1
  }
  correl <- unlist(correl, use.names = FALSE)
  
  counter <- 1
  pair_profit <- list()
  for(pair in pairs) {
    pair_profit[[counter]] <- pair$profit
    counter <- counter + 1
  }
  pair_profit <- unlist(pair_profit, use.names = FALSE)
  
  counter <- 1
  pair_hl <- list()
  for(pair in pairs) {
    pair_hl[[counter]] <- pair$hl
    counter <- counter + 1
  }
  pair_hl <- unlist(pair_hl, use.names = FALSE)
  
  counter <- 1
  pair_margin <- list()
  for(pair in pairs) {
    pair_margin[[counter]] <- pair$margin
    counter <- counter + 1
  }
  pair_margin <- unlist(pair_margin, use.names = FALSE)
  
  returns_failed <- data.frame(pair_returns = pair_returns, perc_failed = perc_failed, pair_profit = pair_profit, pair_hl = pair_hl, pair_margin = pair_margin)
  row.names(returns_failed) <- NULL
  
  return(returns_failed[((returns_failed$pair_returns > min_r) & 
                           (returns_failed$pair_returns < max_r) & 
                           (returns_failed$perc_failed < failed) & 
                           (returns_failed$pair_profit > min_profit) & 
                           (returns_failed$pair_hl > min_hl) & 
                           (returns_failed$pair_hl < max_hl)),])
  
}

prepare_backtest_triple_result <- function(triples, min_r=110, max_r=300, min_profit = 100, failed = 20, min_hl= 7, max_hl = 21) {
  
  counter <- 1
  pair_returns <- list()
  for(pair in triples) {
    pair_returns[[counter]] <- as.numeric(pair$jo_returns$lcr)
    counter <- counter + 1
  }
  pair_returns <- unlist(pair_returns, use.names = FALSE)
  
  counter <- 1
  perc_failed <- list()
  for(pair in triples) {
    perc_failed[[counter]] <- as.numeric(pair$jo_returns$percent_failed)
    counter <- counter + 1
  }
  perc_failed <- unlist(perc_failed, use.names = FALSE)
  
  counter <- 1
  triple_profit <- list()
  for(triple in triples) {
    triple_profit[[counter]] <- triple$profit
    counter <- counter + 1
  }
  triple_profit <- unlist(triple_profit, use.names = FALSE)
  
  counter <- 1
  triple_hl <- list()
  for(triple in triples) {
    triple_hl[[counter]] <- triple$hl
    counter <- counter + 1
  }
  triple_hl <- unlist(triple_hl, use.names = FALSE)
  
  counter <- 1
  triple_margin <- list()
  for(triple in triples) {
    triple_margin[[counter]] <- triple$margin
    counter <- counter + 1
  }
  triple_margin <- unlist(triple_margin, use.names = FALSE)
  
  returns_failed <- data.frame(pair_returns = pair_returns, perc_failed = perc_failed, triple_profit = triple_profit, triple_hl = triple_hl, triple_margin = triple_margin)
  row.names(returns_failed) <- NULL
  
  return(returns_failed[((returns_failed$pair_returns > min_r) & 
                           (returns_failed$pair_returns < max_r) & 
                           (returns_failed$triple_profit > min_profit) & 
                           (returns_failed$perc_failed < failed) & 
                           (returns_failed$triple_hl > min_hl) & 
                           (returns_failed$triple_hl < max_hl)),])
  
}