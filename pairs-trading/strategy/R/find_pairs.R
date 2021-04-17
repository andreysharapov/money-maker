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
          hl <- find_half_life(series)
          if((series[length(series)] < m - num_vars*v)) {
            tradable = 1
            cost <- total_cost_pair(hl = hl, price_1 = price_1, price_2 = price_2, coeff_1 = 1, coeff_2 = johtest@V[2,1], volume = volume, below = TRUE)
          } else if((series[length(series)] > m + num_vars*v)) {
            tradable = 1
            cost <- total_cost_pair(hl = hl, price_1 = price_1, price_2 = price_2, coeff_1 = 1, coeff_2 = johtest@V[2,1], volume = volume, below = FALSE)
          } else {
            if((series[length(series)] < m)) {
              cost <- total_cost_pair(hl = hl, price_1 = price_1, price_2 = price_2, coeff_1 = 1, coeff_2 = johtest@V[2,1], volume = volume, below = TRUE)
            } else if((series[length(series)] > m)) {
              cost <- total_cost_pair(hl = hl, price_1 = price_1, price_2 = price_2, coeff_1 = 1, coeff_2 = johtest@V[2,1], volume = volume, below = FALSE)
            } else {
              throw("Unknown")
            }
          }
          
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

plot_pair <- function(pair, time_index = NULL, add_cut = FALSE, cut_date = Sys.Date()) {
  if(!is.null(time_index)) {
    ts <- as.xts(x = data.frame(series=pair$series), order.by = time_index[(length(time_index) - length(pair$series) + 1):length(time_index)])
  } else {
    ts <- as.xts(x = data.frame(series=pair$series))
  } 
  
  g <- ggplot(ts, aes(x = Index, y = series)) + 
          geom_line() + 
          geom_hline(yintercept=pair$upper, linetype="dashed", color = "red") + 
          geom_hline(yintercept=pair$lower, linetype="dashed", color = "red") + 
          geom_hline(yintercept=(pair$lower + pair$upper)/2, linetype="dashed", color = "blue")
  
  stock_1 <- paste0(str_split(pair$stock_1, "\\.")[[1]][1], '.', str_split(pair$stock_1, "\\.")[[1]][2])
  stock_2 <- paste0(str_split(pair$stock_2, "\\.")[[1]][1], '.', str_split(pair$stock_2, "\\.")[[1]][2])
  
  if(pair$coeff[2] > 0) {
    g <- g + ggtitle(paste0(stock_1, "+", round(abs(pair$coeff[2]), 2), " * ", stock_2))
  } else {
    g <- g + ggtitle(paste0(stock_1, "-", round(abs(pair$coeff[2]), 2), " * ", stock_2))
  }
  
  if(add_cut) {
    g <- g + geom_vline(xintercept=as.Date("2021-04-17"), color = "red")
  }
  print(g)
}

margin_pair <- function(price_1, price_2, coeff_1, coeff_2, volume = 100) {
  volume_1 <- abs(coeff_1) * volume
  volume_2 <- abs(coeff_2) * volume
  margin <- price_1*volume_1*margin_rate + price_2*volume_2*margin_rate
  return(margin)
}

funding_pair <- function(hl, price_1, price_2, coeff_1, coeff_2, volume = 100, below = TRUE) {
  volume_1 <- coeff_1 * volume
  volume_2 <- coeff_2 * volume
  funding <- price_1*volume_1*bbsw*hl/360 + price_2*volume_2*bbsw*hl/360
  if(below) {
    return(funding) 
  } else {
    return(-funding)
  }
}

commission_pair <- function(price_1, price_2, coeff_1, coeff_2, volume = 100) {
  volume_1 <- abs(coeff_1) * volume
  volume_2 <- abs(coeff_2) * volume
  commission <-  2*price_1*volume_1*commission_rate + 2*price_2*volume_2*commission_rate # we do not know the sell price here so just double
  return(commission)
}

total_cost_pair <- function(hl, price_1, price_2, coeff_1, coeff_2, volume = 100, below = TRUE) {
  cost <- funding_pair(hl, price_1, price_2, coeff_1, coeff_2, volume, below) + commission_pair(price_1, price_2, coeff_1, coeff_2, volume)
  return(cost)
}

buy_pair <- function(con, pair) {
  loc_pair <- list(stock_1 = pair$stock_1, stock_2 = pair$stock_2, upper = pair$upper, lower = pair$lower, coeff = pair$coeff, 
                   hl = pair$hl, profit = pair$profit, cost = pair$cost, margin = pair$margin, return = as.numeric(pair$jo_returns$lcr), 
                   failed = pair$jo_returns$percent_failed, buy_date = Sys.Date(), status = 1)
  con$insert(loc_pair)
}

find_all_pair <- function(con, status = 1) {
  query <- paste0("{\"status\" : ", status,"}")
  return(con$find(query))
}

find_pair <- function(con, stock_1, stock_2, status = 1) {
  query <- paste0("{\"stock_1\" : \"", stock_1, "\", \"stock_2\" : \"", stock_2, "\", \"status\" : ", status,"}")
  loc_pair <- con$find(query)
  if(nrow(loc_pair) > 1) {
    print(loc_pair)
    throw("Multiple pairs found")
  }
  
  return(list(stock_1 = loc_pair$stock_1[[1]], 
              stock_2 = loc_pair$stock_2[[1]], 
              upper =   loc_pair$upper[[1]], 
              lower =   loc_pair$lower[[1]], 
              coeff =   loc_pair$coeff[[1]], 
              hl =      loc_pair$hl[[1]], 
              profit =  loc_pair$profit[[1]], 
              cost =    loc_pair$cost[[1]], 
              margin =  loc_pair$margin[[1]], 
              return =  loc_pair$return[[1]], 
              failed =  loc_pair$failed[[1]], 
              buy_date =loc_pair$buy_date[[1]], 
              status =  loc_pair$status[[1]]))
}

sell_pair <- function(con, pair) {
  stock_1 <- pair$stock_1
  stock_2 <- pair$stock_2
  status <- pair$status
  query_1 <- paste0("{\"stock_1\" : \"", stock_1, "\", \"stock_2\" : \"", stock_2, "\", \"status\" : ", status,"}")
  query_2 <- paste0("{\"$set\":{\"status\":", 0, "}}")
  con$update(query_1, query_2)
}

remove_pairs <- function(con)  {
  con$remove('{}')
}

plot_bought_pair <- function(pair, period = 500) {
  
  loc_pair <- pair
  stock_1 <- paste0(str_split(pair$stock_1, "\\.")[[1]][1], '.', str_split(pair$stock_1, "\\.")[[1]][2])
  stock_2 <- paste0(str_split(pair$stock_2, "\\.")[[1]][1], '.', str_split(pair$stock_2, "\\.")[[1]][2])
  
  start_date <- Sys.Date() - period
  s_1 <- getSymbols(stock_1, from = start_date, auto.assign = FALSE)
  s_2 <- getSymbols(stock_2, from = start_date, auto.assign = FALSE)
  
  spread <- as.numeric(pair$coeff[1]) * s_1[, loc_pair$stock_1] + as.numeric(pair$coeff[2]) * s_2[, loc_pair$stock_2]
  names(spread) <- "series"
  loc_pair$series <- spread
  plot_pair(pair=loc_pair, time_index = index(s_1), add_cut = TRUE, cut_date = as.Date(loc_pair$buy_date))
}