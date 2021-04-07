find_pairs <- function(df, period = 360, num_vars = 2, type="eigen", ecdet="none", K=2, spec="longrun") {
  
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
        # look only at r=0 case to make sure a single relationship exists. That's why < in r1 and > in r0
        test_r1 = (johtest@teststat[1] < johtest@cval[1,3])
        test_r0 = (johtest@teststat[2] > johtest@cval[2,3]) 
        if(test_r1 && test_r0) {
          #print("Detected")
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
          pairs[[counter]] <- list(stock_1 = stocks[i], stock_2 = stocks[j], series = series, upper = m + num_vars*v, 
                                   lower = m - num_vars*v, coeff = johtest@V[1:2,1], correl = (cor(data_inp)), hl = hl, profit = num_vars*v, tradable=tradable)
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




