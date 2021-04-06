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

num_years <- 3
today <- Sys.Date()
start_date <- today - 365*num_years
end_date <- today
num_tickers <- 300
exchange <- "ASX"
period <- 500


tickers <- getTicker(exchange=exchange)
working_set <- tickers[0:num_tickers]
getSymbols(working_set, from = start_date)
working_set <- clean_tickers(working_set)
df <- extract_series(working_set = working_set, type = 'Close', order.by = index(get(sample(working_set, 1))))
pairs <- find_pairs(df, period = period)
balanced_pairs <- select_balanced(pairs)
sel_pairs_return <- backtest(pairs = balanced_pairs, order.by = index(get(sample(working_set, 1))), period = period)
bt_df <- prepare_backtest_result(pairs=sel_pairs_return, min_r=110, max_r=300, min_corr=0.7)

sel_pairs_return_tradable <- select_tradable(sel_pairs_return)
bt_df_tradable <- prepare_backtest_result(pairs=sel_pairs_return_tradable, min_r=110, max_r=300, min_corr=0.7)

###########################
pair_index <- 318
plot_pair(sel_pairs_return[[pair_index]], time_index = index(get(sample(working_set, 1))))
plot(as.data.frame(df)[[sel_pairs_return[[pair_index]]$stock_1]] + sel_pairs_return[[pair_index]]$coeff[2]*as.data.frame(df)[[sel_pairs_return[[pair_index]]$stock_2]], type="l")
