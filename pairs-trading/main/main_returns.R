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

# https://www.marketindex.com.au/asx-listed-companies

num_years <- 3
today <- Sys.Date()
start_date <- today - 365*num_years
end_date <- today
num_tickers <- 300
exchange <- "ASX"
period <- 500
confidence_pair <- 5
confidence_triple <- 1
bbsw <- 0.0649 + 0.025 # bbsw + 2.5%
margin_rate <- 0.1
commission_rate <- 0.018

options("getSymbols.warning4.0"=FALSE)
tickers <- getTicker(exchange=exchange)
complete_set <- tickers[0:num_tickers]
working_set <- tickers[0:num_tickers]
getSymbols(working_set, from = start_date) 
working_set <- clean_tickers(complete_set)
for(t in c(1,5,10)) {
  missing <- complete_set[is.na(pmatch(complete_set, working_set))]
  getSymbols(missing, from = start_date) 
  working_set <- clean_tickers(complete_set)
  Sys.sleep(t*60)
}
bad_ones <- refetch_bad_tickers(working_set)  
if(length(bad_ones) > 1) {
  getSymbols(bad_ones, from = start_date)  
}

df <- extract_series(working_set = working_set, type = 'Close', order.by = index(get(sample(working_set, 1))))
if(!(any(is.na(df) | is.infinite(df) | is.null(df)))) {
  pairs <- find_pairs(df, period = period, confidence = confidence_pair)
  triples <- find_triples(df, period = period, confidence = confidence_triple)
  
  balanced_pairs <- select_balanced(pairs); sel_pairs_tradable <- select_tradable(balanced_pairs) # select pairs that do not have very large or very small coefficients
  balanced_triples <- select_balanced_triples(triples); sel_triples_tradable <- select_tradable(balanced_triples)
  
  sel_pairs_return <- backtest(pairs = sel_pairs_tradable, order.by = index(get(sample(working_set, 1))), period = period, confidence = confidence_pair, backtest_type = "pair")
  sel_triples_return <- backtest(pairs = sel_triples_tradable, order.by = index(get(sample(working_set, 1))), period = period, confidence = confidence_triple, backtest_type = "triple")
  
  bt_df_pair_tradable <- prepare_backtest_result(pairs=sel_pairs_return, min_r=10, max_r=3000,  min_profit = 10, failed = 20, min_hl = 7, max_hl = 21)
  bt_df_triple_tradable <- prepare_backtest_triple_result(triples=sel_triples_return, min_r=10, max_r=3000, min_profit = 100, failed = 5, min_hl = 7, max_hl = 14)  
} else{
  print("ERROR")
}

buy_pair(sel_pairs_return[[64]])
buy_pair(sel_pairs_return[[8]])

found_pairs <- find_all_pair()
found_pair <- find_pair("VTS.AX.Close", "RMC.AX.Close")
plot_bought_pair(found_pair)
sell_pair(found_pair)
remove_pairs()

buy_triple(sel_triples_return[[5136]])
buy_triple(sel_triples_return[[3714]])

found_triples <- find_all_triple()
found_triple <- find_triple("HUB.AX.Close", "CUV.AX.Close", "CQE.AX.Close")
plot_bought_triple(found_triple)
sell_triple(found_triple)
remove_triples()

########################### tradable pairs
pair_index <- 64
plot_pair(sel_pairs_return[[pair_index]], time_index = index(get(sample(working_set, 1))))
plot(as.data.frame(df)[[sel_pairs_return[[pair_index]]$stock_1]] + sel_pairs_return[[pair_index]]$coeff[2]*as.data.frame(df)[[sel_pairs_return[[pair_index]]$stock_2]], type="l")

########################### tradable triples
pair_index <- 5136
plot_triple(sel_triples_return[[pair_index]], time_index = index(get(sample(working_set, 1))))
plot(as.data.frame(df)[[sel_triples_return[[pair_index]]$stock_1]] + 
       sel_triples_return[[pair_index]]$coeff[2]*as.data.frame(df)[[sel_triples_return[[pair_index]]$stock_2]] + 
       sel_triples_return[[pair_index]]$coeff[3]*as.data.frame(df)[[sel_triples_return[[pair_index]]$stock_3]], type="l")

### bought GOZ.AX.Close CMW.AX.Close
### bought DXS.AX.Close CWN.AX.Close
### bought SPK.AX.Close WOW.AX.Close
### bought TPG.AX.Close FLT.AX.Close
### bought CWN.AX.Close VCX.AX.Close
### bought CBA.AX.Close TLS.AX.Close SEK.AX.Close

found_pair <- find_by_name(pairs = pairs, stock_1 = "GOZ.AX.Close", stock_2 = "CMW.AX.Close")
is.null(found_pair)
plot_pair(found_pair, time_index = index(get(sample(working_set, 1))))
plot(as.data.frame(df)[[found_pair$stock_1]] + found_pair$coeff[2]*as.data.frame(df)[[found_pair$stock_2]], type="l")

found_pair <- find_by_name(pairs = pairs, stock_1 = "DXS.AX.Close", stock_2 = "CWN.AX.Close")
is.null(found_pair)
plot_pair(found_pair, time_index = index(get(sample(working_set, 1))))
plot(as.data.frame(df)[[found_pair$stock_1]] + found_pair$coeff[2]*as.data.frame(df)[[found_pair$stock_2]], type="l")

found_pair <- find_by_name(pairs = pairs, stock_1 = "SPK.AX.Close", stock_2 = "WOW.AX.Close")
is.null(found_pair)
plot_pair(found_pair, time_index = index(get(sample(working_set, 1))))
plot(as.data.frame(df)[["WOW.AX.Close"]] - 9.87*as.data.frame(df)[["SPK.AX.Close"]], type="l")

found_pair <- find_by_name(pairs = pairs, stock_1 = "TPG.AX.Close", stock_2 = "FLT.AX.Close")
is.null(found_pair)
plot_pair(found_pair, time_index = index(get(sample(working_set, 1))))
plot(as.data.frame(df)[[found_pair$stock_1]] + found_pair$coeff[2]*as.data.frame(df)[[found_pair$stock_2]], type="l")

found_pair <- find_by_name(pairs = pairs, stock_1 = "CWN.AX.Close", stock_2 = "VCX.AX.Close")
is.null(found_pair)
plot_pair(found_pair, time_index = index(get(sample(working_set, 1))))
plot(as.data.frame(df)[[found_pair$stock_1]] + found_pair$coeff[2]*as.data.frame(df)[[found_pair$stock_2]], type="l")

found_triple <- find_by_triple_name(triples = triples, stock_1 = "CBA.AX.Close", stock_2 = "TLS.AX.Close", stock_3 = "SEK.AX.Close")
is.null(found_triple)
plot_triple(found_triple, time_index = index(get(sample(working_set, 1))))
plot(as.data.frame(df)[[found_triple$stock_1]] + 
       found_triple$coeff[2]*as.data.frame(df)[[found_triple$stock_2]] + 
       found_triple$coeff[3]*as.data.frame(df)[[found_triple$stock_3]], type="l")

plot(as.data.frame(df_t)[["CBA.AX.Close"]] + 
       found_triple$coeff[2]*as.data.frame(df_t)[["TLS.AX.Close"]] + 
       found_triple$coeff[3]*as.data.frame(df_t)[["SEK.AX.Close"]], type="l")
###############################################

