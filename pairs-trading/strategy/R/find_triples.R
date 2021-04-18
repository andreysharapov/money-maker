# https://quant.stackexchange.com/questions/2076/how-to-interpret-the-eigenmatrix-from-a-johansen-cointegration-test
# https://stats.stackexchange.com/questions/186208/johansen-test-for-cointegration
# http://denizstij.blogspot.com/2013/11/cointegration-tests-adf-and-johansen.html
# https://www.quantstart.com/articles/Johansen-Test-for-Cointegrating-Time-Series-Analysis-in-R/
find_triples <- function(df, period = 360, num_vars = 2, type="eigen", ecdet="none", K=2, spec="longrun", confidence=1, volume = 100) {
  
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
            price_1 <- as.numeric(last(df[(dl-period + 1):dl,i]))
            price_2 <- as.numeric(last(df[(dl-period + 1):dl,j]))
            price_3 <- as.numeric(last(df[(dl-period + 1):dl,k]))
            series <- as.data.frame(df[(dl-period + 1):dl,i])+johtest@V[2,1]*as.data.frame(df[(dl-period +1):dl,j]) + johtest@V[3,1]*as.data.frame(df[(dl-period +1):dl,k]) # multiply by the first eigenvector, since it corresponds to the highest eigenvalue. it is always 1 and x
            series <- series[,1]
            m = mean(series, na.rm=TRUE)
            v = sd(series, na.rm=TRUE)
            # check we it is outside of num_vars standard deviations. If so, it is worth trading
            tradable = 0
            hl <- find_half_life(series)
            if((series[length(series)] < m - num_vars*v)) {
              tradable = 1
              cost <- total_cost_triple(hl = hl, price_1 = price_1, price_2 = price_2, price_3 = price_3, 
                                        coeff_1 = 1, coeff_2 = johtest@V[2,1], coeff_3 = johtest@V[3,1], volume = volume, below = TRUE)
            } else if((series[length(series)] > m + num_vars*v)) {
              tradable = 1
              cost <- total_cost_triple(hl = hl, price_1 = price_1, price_2 = price_2, price_3 = price_3, 
                                        coeff_1 = 1, coeff_2 = johtest@V[2,1], coeff_3 = johtest@V[3,1], volume = volume, below = FALSE)
            } else {
              if((series[length(series)] < m)) {
                cost <- total_cost_triple(hl = hl, price_1 = price_1, price_2 = price_2, price_3 = price_3, 
                                          coeff_1 = 1, coeff_2 = johtest@V[2,1], coeff_3 = johtest@V[3,1], volume = volume, below = TRUE)
              } else if((series[length(series)] > m)) {
                cost <- total_cost_triple(hl = hl, price_1 = price_1, price_2 = price_2, price_3 = price_3, 
                                          coeff_1 = 1, coeff_2 = johtest@V[2,1], coeff_3 = johtest@V[3,1], volume = volume, below = FALSE)
              } else {
                throw("Unknown")
              }
            }
            
            margin <- margin_triple(price_1 = price_1, price_2 = price_2, price_3 = price_3, 
                                    coeff_1 = 1, coeff_2 = johtest@V[2,1], coeff_3 = johtest@V[3,1], volume = volume)
            pairs[[counter]] <- list(stock_1 = stocks[i], stock_2 = stocks[j], stock_3 = stocks[k], series = series, upper = m + num_vars*v, 
                                     lower = m - num_vars*v, coeff = johtest@V[1:3,1], correl = (cor(data_inp)), hl = hl, 
                                     profit = volume*num_vars*v - cost, tradable=tradable, cost = cost, margin = margin)
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

margin_triple <- function(price_1, price_2, price_3, coeff_1, coeff_2, coeff_3, volume = 100) {
  volume_1 <- abs(coeff_1) * volume
  volume_2 <- abs(coeff_2) * volume
  volume_3 <- abs(coeff_3) * volume
  margin <- price_1*volume_1*margin_rate + price_2*volume_2*margin_rate + price_3*volume_3*margin_rate
  return(margin)
}

funding_triple <- function(hl, price_1, price_2, price_3, coeff_1, coeff_2, coeff_3, volume = 100, below = TRUE) {
  volume_1 <- coeff_1 * volume
  volume_2 <- coeff_2 * volume
  volume_3 <- coeff_3 * volume
  funding <- price_1*volume_1*bbsw*hl/360 + price_2*volume_2*bbsw*hl/360 + price_3*volume_3*bbsw*hl/360
  if(below) {
    return(funding) 
  } else {
    return(-funding)
  }
}

commission_triple <- function(price_1, price_2, price_3, coeff_1, coeff_2, coeff_3, volume = 100) {
  volume_1 <- abs(coeff_1) * volume
  volume_2 <- abs(coeff_2) * volume
  volume_3 <- abs(coeff_3) * volume
  commission <-  2*price_1*volume_1*commission_rate + 2*price_2*volume_2*commission_rate + 2*price_3*volume_3*commission_rate# we do not know the sell price here so just double
  return(commission)
}

total_cost_triple <- function(hl, price_1, price_2, price_3, coeff_1, coeff_2, coeff_3, volume = 100, below = TRUE) {
  cost <- funding_triple(hl, price_1, price_2, price_3, coeff_1, coeff_2, coeff_3, volume, below) + 
    commission_triple(price_1, price_2, price_3, coeff_1, coeff_2, coeff_3, volume)
  return(cost)
}

buy_triple <- function(triple) {
  con <- mongo(collection = "triples", db = "strategy")
  loc_triple <- list(stock_1 = triple$stock_1, stock_2 = triple$stock_2, stock_3 = triple$stock_3, upper = triple$upper, lower = triple$lower, coeff = triple$coeff, 
                   hl = triple$hl, profit = triple$profit, cost = triple$cost, margin = triple$margin, return = as.numeric(triple$jo_returns$lcr), 
                   failed = triple$jo_returns$percent_failed, buy_date = Sys.Date(), status = 1)
  con$insert(loc_triple)
}

find_all_triple <- function(status = 1) {
  con <- mongo(collection = "triples", db = "strategy")
  query <- paste0("{\"status\" : ", status,"}")
  return(con$find(query))
}

find_triple <- function(stock_1, stock_2, stock_3, status = 1) {
  con <- mongo(collection = "triples", db = "strategy")
  query <- paste0("{\"stock_1\" : \"", stock_1, "\", \"stock_2\" : \"", stock_2, "\", \"stock_3\" : \"", stock_3, "\", \"status\" : ", status,"}")
  loc_triple <- con$find(query)
  if(nrow(loc_triple) > 1) {
    print(loc_triple)
    throw("Multiple pairs found")
  }
  
  if(nrow(loc_triple) == 0) {
    throw("Nothing found")
  }
  
  return(list(stock_1 = loc_triple$stock_1[[1]], 
              stock_2 = loc_triple$stock_2[[1]], 
              stock_3 = loc_triple$stock_3[[1]], 
              upper =   loc_triple$upper[[1]], 
              lower =   loc_triple$lower[[1]], 
              coeff =   loc_triple$coeff[[1]], 
              hl =      loc_triple$hl[[1]], 
              profit =  loc_triple$profit[[1]], 
              cost =    loc_triple$cost[[1]], 
              margin =  loc_triple$margin[[1]], 
              return =  loc_triple$return[[1]], 
              failed =  loc_triple$failed[[1]], 
              buy_date =loc_triple$buy_date[[1]], 
              status =  loc_triple$status[[1]]))
}

sell_triple <- function(triple) {
  con <- mongo(collection = "triples", db = "strategy")
  stock_1 <- triple$stock_1
  stock_2 <- triple$stock_2
  stock_3 <- triple$stock_3
  status <- triple$status
  query_1 <- paste0("{\"stock_1\" : \"", stock_1, "\", \"stock_2\" : \"", stock_2, "\", \"stock_3\" : \"", stock_3, "\",\"status\" : ", status,"}")
  query_2 <- paste0("{\"$set\":{\"status\":", 0, "}}")
  con$update(query_1, query_2)
}

remove_triples <- function()  {
  con <- mongo(collection = "triples", db = "strategy")
  con$remove('{}')
}

plot_bought_triple <- function(triple, period = 1000) {
  
  loc_triple <- triple
  stock_1 <- paste0(str_split(triple$stock_1, "\\.")[[1]][1], '.', str_split(triple$stock_1, "\\.")[[1]][2])
  stock_2 <- paste0(str_split(triple$stock_2, "\\.")[[1]][1], '.', str_split(triple$stock_2, "\\.")[[1]][2])
  stock_3 <- paste0(str_split(triple$stock_3, "\\.")[[1]][1], '.', str_split(triple$stock_3, "\\.")[[1]][2])
  
  start_date <- Sys.Date() - period
  s_1 <- getSymbols(stock_1, from = start_date, auto.assign = FALSE)
  s_2 <- getSymbols(stock_2, from = start_date, auto.assign = FALSE)
  s_3 <- getSymbols(stock_3, from = start_date, auto.assign = FALSE)
  
  spread <- as.numeric(triple$coeff[1]) * s_1[, loc_triple$stock_1] + as.numeric(triple$coeff[2]) * s_2[, loc_triple$stock_2] + as.numeric(triple$coeff[3]) * s_3[, loc_triple$stock_3]
  names(spread) <- "series"
  loc_triple$series <- spread
  plot_triple(pair=loc_triple, time_index = index(s_1), add_cut = TRUE, cut_date = as.Date(loc_triple$buy_date))
}

