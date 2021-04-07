getTicker <- function(exchange="ASX") {
  data <- read.csv2(file=system.file("extdata", paste0(exchange,".csv"), package="datatools"), header = TRUE, sep = ',')
  tickers <- data$ticker
  if(exchange == 'ASX') {
    tickers <- unlist(lapply(tickers, function(x) {paste0(x,'.AX')}), use.names = FALSE)
  }
  return(tickers)
}

clean_tickers <- function(working_set) {
  b_days_list <- list()
  for(ticker in working_set) {
    if(exists(ticker)) {
      b_days_list[[ticker]] <- dim(get(ticker))[1] 
    }
  }
  b_days <- max(unlist(b_days_list, use.names = FALSE))
  
  for(ticker in working_set) {
    if(!exists(ticker) || (dim(get(ticker))[1] < b_days)) {
      working_set <- working_set[working_set != ticker]
    }
  }
  
  return(working_set)

}

getSymbolBatches <- function(batches, batch_size = 100, time_wait = 60) {
  for(batch in 1:batches) {
    getSymbols(working_set[(batch_size*(batch - 1)):(batch_size*batch)], from = start_date)
    Sys.sleep(time_wait)
  }
}

select_balanced <- function(pairs, lower = 0.02, upper = 50) {
  
  sel_pairs <- list()
  num_pairs <- 1
  for(pair in pairs) {
    if((abs(as.numeric(pair$coeff[2])) < 50) && (abs(as.numeric(pair$coeff[2])) > 0.02)) {
      sel_pairs[[num_pairs]] <- pair
      num_pairs <- num_pairs + 1  
    }
  }
  print(num_pairs - 1)
  return(sel_pairs)
}

select_tradable <- function(pairs) {
  sel_pairs <- list()
  num_pairs <- 1
  
  for(pair in pairs) {
    if(pair$tradable == 1) { 
      sel_pairs[[num_pairs]] <- pair
      num_pairs <- num_pairs + 1
    }
  }
  
  print(num_pairs-1)
  
  return(sel_pairs)
}
