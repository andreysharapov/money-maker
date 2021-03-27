find_pairs <- function(df, past_days = 130, num_vars = 2, type="eigen", ecdet="none", K=2, spec="longrun") {
  
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
        johtest<-ca.jo(as.data.frame(cbind(df[[stocks[i]]], df[[stocks[j]]])),type=type,ecdet=ecdet,K=K,spec=spec)
        # look only at r=0 case to make sure a single relationship exists. That's why < in r1 and > in r0
        test_r1 = (johtest@teststat[1] < johtest@cval[1,3])
        test_r0 = (johtest@teststat[2] > johtest@cval[2,3]) 
        if(test_r1 && test_r0) {
          #print("Detected")
          series <- df[(dl-past_days):dl,i]+johtest@V[2,1]*df[(dl-past_days):dl,j] # multiply by the first eigenvector, since it corresponds to the highest eigenvalue. it is always 1 and x
          m = mean(series)
          v = sqrt(var(series))
          # check we it is outside of num_vars standard deviations. If so, it is worth trading
          if((series[length(series)] > m + num_vars*v) || (series[length(series)] < m - num_vars*v)) {
            hl <- find_half_life(series)
            pairs[[counter]] <- list(stock_1 = stocks[i], stock_2 = stocks[j], series = series, upper = m + num_vars*v, 
                                     lower = m - num_vars*v, coeff = johtest@V[1:2,1], correl = (cor(df[[stocks[i]]], df[[stocks[j]]])), hl = hl)
            counter <- counter + 1
          }
        }
      }
    }
  }
  
  return(pairs)
}

plot_pair <- function(pair, time_index) {
  ts <- as.xts(x = data.frame(series=pair$series), order.by = time_index[(length(time_index) - length(pair$series) + 1):length(time_index)])
  g <- ggplot(ts, aes(x = Index, y = series)) + 
          geom_line() + 
          geom_hline(yintercept=pair$upper, linetype="dashed", color = "red") + 
          geom_hline(yintercept=pair$lower, linetype="dashed", color = "red") + 
          geom_hline(yintercept=(pair$lower + pair$upper)/2, linetype="dashed", color = "blue")
  
  if(pair$coeff[2] > 0) {
    g <- g + ggtitle(paste0(pair$stock_1, "+", round(abs(pair$coeff[2]), 2), " * ", pair$stock_2, "  corr = ", round(pair$correl, 2)))
  } else {
    g <- g + ggtitle(paste0(pair$stock_1, "-", round(abs(pair$coeff[2]), 2), " * ", pair$stock_2, "  corr = ", round(pair$correl, 2)))
  }
  print(g)
}




