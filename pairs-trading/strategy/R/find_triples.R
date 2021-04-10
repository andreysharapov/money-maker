# https://quant.stackexchange.com/questions/2076/how-to-interpret-the-eigenmatrix-from-a-johansen-cointegration-test
# https://stats.stackexchange.com/questions/186208/johansen-test-for-cointegration
# http://denizstij.blogspot.com/2013/11/cointegration-tests-adf-and-johansen.html
# https://www.quantstart.com/articles/Johansen-Test-for-Cointegrating-Time-Series-Analysis-in-R/
find_triples <- function(df, period = 360, num_vars = 2, type="eigen", ecdet="none", K=2, spec="longrun", confidence=1) {
  
  stocks <- names(df)
  dl = dim(df)[1]
  
  counter = 1
  pairs <- list()
  
  for(i in 1:length(stocks)) {
    for(j in 1:length(stocks)) {
      for(k in 1:length(stocks)) {
        if((j>i) && (k>j) && (k>i)) {
          data_inp <-  df[(dl-period+1):dl, c(stocks[i], stocks[j], stocks[k])]
          if((length(unique(data_inp[,1])) == 1) || (length(unique(data_inp[,2])) == 1) || (length(unique(data_inp[,3])) == 1)) {
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
          test_r2 = (johtest@teststat[1] < johtest@cval[1,test_stat_col])
          test_r1 = (johtest@teststat[2] < johtest@cval[2,test_stat_col])
          test_r0 = (johtest@teststat[3] > johtest@cval[3,test_stat_col]) 
          if(test_r2 && test_r1 && test_r0) {
            #print("Detected")
            series <- as.data.frame(df[(dl-period + 1):dl,i])+johtest@V[2,1]*as.data.frame(df[(dl-period +1):dl,j]) + johtest@V[3,1]*as.data.frame(df[(dl-period +1):dl,k]) # multiply by the first eigenvector, since it corresponds to the highest eigenvalue. it is always 1 and x
            series <- series[,1]
            m = mean(series, na.rm=TRUE)
            v = sd(series, na.rm=TRUE)
            # check we it is outside of num_vars standard deviations. If so, it is worth trading
            tradable = 0
            if((series[length(series)] > m + num_vars*v) || (series[length(series)] < m - num_vars*v)) {
              tradable = 1
            }
            hl <- find_half_life(series)
            pairs[[counter]] <- list(stock_1 = stocks[i], stock_2 = stocks[j], stock_3 = stocks[k], series = series, upper = m + num_vars*v, 
                                     lower = m - num_vars*v, coeff = johtest@V[1:3,1], correl = (cor(data_inp)), hl = hl, profit = num_vars*v, tradable=tradable)
            counter <- counter + 1
          }
        }
      }
    }
  }
  
  return(pairs)
}

plot_triple <- function(pair, time_index, add_cut = FALSE, cut_date = Sys.Date()) {
  ts <- as.xts(x = data.frame(series=pair$series), order.by = time_index[(length(time_index) - length(pair$series) + 1):length(time_index)])
  g <- ggplot(ts, aes(x = Index, y = series)) + 
    geom_line() + 
    geom_hline(yintercept=pair$upper, linetype="dashed", color = "red") + 
    geom_hline(yintercept=pair$lower, linetype="dashed", color = "red") + 
    geom_hline(yintercept=(pair$lower + pair$upper)/2, linetype="dashed", color = "blue")
  
  stock_1 <- paste0(str_split(pair$stock_1, "\\.")[[1]][1], '.', str_split(pair$stock_1, "\\.")[[1]][2])
  stock_2 <- paste0(str_split(pair$stock_2, "\\.")[[1]][1], '.', str_split(pair$stock_2, "\\.")[[1]][2])
  stock_3 <- paste0(str_split(pair$stock_3, "\\.")[[1]][1], '.', str_split(pair$stock_3, "\\.")[[1]][2])
  
  if(pair$coeff[2] > 0) {
    if(pair$coeff[3] > 0) {
      g <- g + ggtitle(paste0(stock_1, "+", round(abs(pair$coeff[2]), 2), " * ", stock_2, "+" , round(abs(pair$coeff[3]), 2), " * ", stock_3)) 
    } else {
      g <- g + ggtitle(paste0(stock_1, "+", round(abs(pair$coeff[2]), 2), " * ", stock_2, "-" , round(abs(pair$coeff[3]), 2), " * ", stock_3)) 
    }
  } else {
    if(pair$coeff[3] > 0) {
      g <- g + ggtitle(paste0(stock_1, "-", round(abs(pair$coeff[2]), 2), " * ", stock_2, "+" , round(abs(pair$coeff[3]), 2), " * ", stock_3))  
    } else {
      g <- g + ggtitle(paste0(stock_1, "-", round(abs(pair$coeff[2]), 2), " * ", stock_2, "-" , round(abs(pair$coeff[3]), 2), " * ", stock_3))
    }
  }
  
  if(add_cut) {
    g <- g + geom_vline(xintercept=cut_date, color = "red")
  }
  print(g)
}




