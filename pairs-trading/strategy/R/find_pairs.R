find_pairs <- function(df, period = 360, num_vars = 2, type="eigen", ecdet="none", K=2, spec="longrun", confidence=1, volume = 100) {
  
  stocks <- names(df)
  dl = dim(df)[1]
  
  counter = 1
  pairs <- list()
    
  for(i in 1:length(stocks))
  {
    for(j in 1:length(stocks))
    {
      if(j>i)
      {
        data_inp <-  df[(dl-period+1):dl, c(stocks[i], stocks[j])]
        if((length(unique(data_inp[,1])) == 1) || (length(unique(data_inp[,2])) == 1)) {
          next
        }
        johtest <- tryCatch({
          ca.jo(data_inp,type=type,ecdet=ecdet,K=K,spec=spec)
        }, error = function(x) {
          return(NULL)
        })
        if(is.null(johtest)) {
          next
        }
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
        # look only at r=0 case to make sure a single relationship exists. That's why < in r1 and > in r0
        test_r1 = (johtest@teststat[1] < johtest@cval[1,test_stat_col])
        test_r0 = (johtest@teststat[2] > johtest@cval[2,test_stat_col]) 
        if(test_r1 && test_r0) {
          #print("Detected")
          price_1 <- as.numeric(last(df[(dl-period + 1):dl,i]))
          price_2 <- as.numeric(last(df[(dl-period + 1):dl,j]))
          series <- as.data.frame(df[(dl-period + 1):dl,i])+johtest@V[2,1]*as.data.frame(df[(dl-period +1):dl,j]) # multiply by the first eigenvector, since it corresponds to the highest eigenvalue. it is always 1 and x
          series <- series[,1]
          m = mean(series, na.rm=TRUE)
          v = sd(series, na.rm=TRUE)
          # check we it is outside of num_vars standard deviations. If so, it is worth trading
          tradable = 0
          if((series[length(series)] > m + num_vars*v) || (series[length(series)] < m - num_vars*v)) {
            tradable = 1
          }
          hl <- find_half_life(series)
          cost <- total_cost_pair(hl = hl, price_1 = price_1, price_2 = price_2, coeff_1 = 1, coeff_2 = johtest@V[2,1], volume = volume)
          margin <- margin_pair(price_1 = price_1, price_2 = price_2, coeff_1 = 1, coeff_2 = johtest@V[2,1], volume = volume)
          pairs[[counter]] <- list(stock_1 = stocks[i], stock_2 = stocks[j], series = series, upper = m + num_vars*v, 
                                   lower = m - num_vars*v, coeff = johtest@V[1:2,1], correl = (cor(data_inp)), hl = hl, 
                                   profit = volume*num_vars*v - cost, tradable=tradable, cost = cost, margin = margin)
          counter <- counter + 1
        }
      }
    }
  }
  
  return(pairs)
}

plot_pair <- function(pair, time_index, add_cut = FALSE, cut_date = Sys.Date()) {
  ts <- as.xts(x = data.frame(series=pair$series), order.by = time_index[(length(time_index) - length(pair$series) + 1):length(time_index)])
  g <- ggplot(ts, aes(x = Index, y = series)) + 
          geom_line() + 
          geom_hline(yintercept=pair$upper, linetype="dashed", color = "red") + 
          geom_hline(yintercept=pair$lower, linetype="dashed", color = "red") + 
          geom_hline(yintercept=(pair$lower + pair$upper)/2, linetype="dashed", color = "blue")
  
  stock_1 <- paste0(str_split(pair$stock_1, "\\.")[[1]][1], '.', str_split(pair$stock_1, "\\.")[[1]][2])
  stock_2 <- paste0(str_split(pair$stock_2, "\\.")[[1]][1], '.', str_split(pair$stock_2, "\\.")[[1]][2])
  
  if(pair$coeff[2] > 0) {
    g <- g + ggtitle(paste0(stock_1, "+", round(abs(pair$coeff[2]), 2), " * ", stock_2, "  corr = ", round(pair$correl[1,2], 2)))
  } else {
    g <- g + ggtitle(paste0(stock_1, "-", round(abs(pair$coeff[2]), 2), " * ", stock_2, "  corr = ", round(pair$correl[1,2], 2)))
  }
  
  if(add_cut) {
    g <- g + geom_vline(xintercept=cut_date, color = "red")
  }
  print(g)
}

margin_pair <- function(price_1, price_2, coeff_1, coeff_2, volume = 100) {
  volume_1 <- abs(coeff_1) * volume
  volume_2 <- abs(coeff_2) * volume
  margin <- price_1*volume_1*margin_rate + price_2*volume_2*margin_rate
  return(margin)
}

funding_pair <- function(hl, price_1, price_2, coeff_1, coeff_2, volume = 100) {
  volume_1 <- abs(coeff_1) * volume
  volume_2 <- abs(coeff_2) * volume
  funding <- price_1*volume_1*bbsw*hl/360 + price_2*volume_2*bbsw*hl/360
  return(funding)
}

commission_pair <- function(price_1, price_2, coeff_1, coeff_2, volume = 100) {
  volume_1 <- abs(coeff_1) * volume
  volume_2 <- abs(coeff_2) * volume
  commission <-  2*price_1*volume_1*commission_rate + 2*price_2*volume_2*commission_rate # we do not know the sell price here so just double
  return(commission)
}

total_cost_pair <- function(hl, price_1, price_2, coeff_1, coeff_2, volume = 100) {
  cost <- funding_pair(hl, price_1, price_2, coeff_1, coeff_2, volume) + commission_pair(price_1, price_2, coeff_1, coeff_2, volume)
  return(cost)
}
