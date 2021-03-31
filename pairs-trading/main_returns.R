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

num_years <- 4
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
