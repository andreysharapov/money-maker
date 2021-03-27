getTicker <- function(exchange="ASX") {
  data <- read.csv2(file=system.file("extdata", paste0(exchange,".csv"), package="datatools"), header = TRUE, sep = ',')
  tickers <- data$ticker
  if(exchange == 'ASX') {
    tickers <- unlist(lapply(tickers, function(x) {paste0(x,'.AX')}), use.names = FALSE)
  }
  return(tickers)
}