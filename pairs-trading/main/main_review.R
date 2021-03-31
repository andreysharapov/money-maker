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
library(R.oo)

pairs_col_review = mongo(collection = "pairs", db = "strategy")
pairs_review <- pairs_col_review$find('{}')
buy_date <- unique(unlist(pairs_review$buy_date, use.names = FALSE))
add_days <- as.numeric(difftime(Sys.Date(), as.Date(buy_date), units = 'days'))

stocks_1 <- unlist(pairs_review$stock_1, use.names=FALSE)
stocks_2 <- unlist(pairs_review$stock_2, use.names=FALSE)
all_stocks <- unique(c(stocks_1, stocks_2))

num_years <- 2
today <- Sys.Date()
start_date <- today - 365*num_years - add_days
end_date <- today
exchange <- "ASX"
past_days = 200 + add_days

load_quantlib_calendars('Australia', from=as.character(today - 365*(num_years + 1)), to=as.character(today + 10))
b_days <- bizdays(as.character(start_date), as.character(end_date), 'QuantLib/Australia')

working_set <- unlist(lapply(all_stocks, function(x) {return(paste0(str_split(x, "\\.")[[1]][1], '.', str_split(x, "\\.")[[1]][2]))}), use.names = FALSE)
getSymbols(working_set, from = start_date)

for(ticker in working_set) {
  if(!exists(ticker) || (dim(get(ticker))[1] < b_days)) {
    working_set <- working_set[working_set != ticker]
  }
}

df <- extract_series(working_set = working_set, type = 'Close', order.by = index(get(sample(working_set, 1))))
dl = dim(df)[1]
pairs <- list()
counter <- 1
for(i in 1:length(working_set)) {
  series <- df[[pairs_review$stock_1[[i]]]][(dl-past_days):dl]+pairs_review$coeff[[i]][2]*df[[pairs_review$stock_2[[i]]]][(dl-past_days):dl]
  pairs[[counter]] <- list(stock_1 = pairs_review$stock_1[[i]], stock_2 = pairs_review$stock_2[[i]], series = series, upper = pairs_review$upper[[i]], 
                           lower = pairs_review$lower[[i]], coeff = pairs_review$coeff[[i]], correl = pairs_review$correl[[i]], hl = pairs_review$hl[[i]])
  counter <- counter + 1
}

pdf(file="/home/andrey/plots/plots.pdf")  
for(pair in pairs) {
  plot_pair(pair, time_index = index(get(sample(working_set, 1))), add_cut = TRUE, cut_date = as.Date(buy_date))  
}
dev.off() 

##################################
pair <- pairs[[23]]
price.pair <- xts(df[, c(pair$stock_1, pair$stock_2)], order.by = index(get(sample(working_set, 1))))
reg <- EstimateParametersJo(price.pair)
str(reg)
plot(reg$spread, type="l")

params <- EstimateParametersHistoricallyJo(price.pair, period = 180)
signal <- SimpleJo(params$spread, params$stddev)

barplot(signal,col="blue",space = 0, border = "blue",xaxt="n",yaxt="n",xlab="",ylab="")
par(new=TRUE)
plot(params$spread)

return.pairtrading <- Return(price.pair, lag(signal), lag(params$hedge.ratio))
plot(100 * cumprod(1 + return.pairtrading))

##############################


price.pair <- xts(df[, c(pair$stock_1, pair$stock_2)], order.by = index(get(sample(working_set, 1))))
reg <- EstimateParameters(price.pair)
str(reg)
plot(reg$spread, type="l")

params <- EstimateParametersHistorically(price.pair, period = 180)
signal <- Simple(params$spread, 0.05)

barplot(signal,col="blue",space = 0, border = "blue",xaxt="n",yaxt="n",xlab="",ylab="")
par(new=TRUE)
plot(params$spread)

return.pairtrading <- Return(price.pair, lag(signal), lag(params$hedge.ratio))
plot(100 * cumprod(1 + return.pairtrading))
