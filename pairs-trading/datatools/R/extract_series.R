extract_series <- function(working_set, type = 'Close', order.by) {
  
  df <- NULL
  for(i in 1:length(working_set)) {
    x <- get(working_set[i])
    if(type == "Close") {
      if(is.null(df)) {
        df <- data.frame(coredata(Cl(x)))
      } else {
        df <- cbind(df, coredata(Cl(x)))
      }
    } else if(type == "Open") {
      if(is.null(df)) {
        df <- data.frame(coredata(Cl(x)))
      } else {
        df <- cbind(df, coredata(Cl(x)))
      }
    } else {
      throw('Unknown type')
    }
  }
  df <- xts(df, order.by = order.by)
  return(df)
}