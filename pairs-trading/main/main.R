library(datatools)
library(quantmod)
library(urca)
library(strategy)
library(ggplot2)
library(rlist)
library(bizdays)
library(RQuantLib)
library(mongolite)
library(stringr)
library(PairTrading)

num_years <- 2
today <- Sys.Date()
start_date <- today - 365*num_years
end_date <- today
num_tickers <- 500
batches <- 5
exchange <- "ASX"
past_days = 200

load_quantlib_calendars('Australia', from=as.character(today - 365*(num_years + 1)), to=as.character(today + 10))
b_days <- bizdays(as.character(start_date), as.character(end_date), 'QuantLib/Australia')

tickers <- getTicker(exchange=exchange)
working_set <- tickers[0:num_tickers]

for(batch in 1:batches) {
  getSymbols(working_set[(100*(batch - 1)):(100*batch)], from = start_date)
  Sys.sleep(60)
}


for(ticker in working_set) {
  if(!exists(ticker) || (dim(get(ticker))[1] < b_days)) {
    working_set <- working_set[working_set != ticker]
  }
}

df <- extract_series(working_set = working_set, type = 'Close', order.by = index(get(sample(working_set, 1))))
pairs <- find_pairs(df, past_days = past_days)
list.order(pairs, correl)

for(pair in pairs) {
  if(abs(pair$coeff[2]) < 10) {
    plot_pair(pair, time_index = index(get(sample(working_set, 1))))  
  }
}

sel_pairs <- list()
num_pairs <- 1
for(pair in pairs) {
  if(abs(pair$coeff[2]) < 5) {
    sel_pairs[[num_pairs]] <- pair
    num_pairs <- num_pairs + 1 
  }
}


pairs_col = mongo(collection = "pairs", db = "strategy")
for(pair in pairs) {
  pair$buy_date <- Sys.Date()
  pairs_col$insert(pair)
}

plot_pair(sel_pairs[[1]], time_index = index(get(sample(working_set, 1)))) 

