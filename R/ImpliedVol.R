#' Use black76 model to compute the implied volatility of a set of European options on future contracts.
#' This function - ImpliedVol will return dataframe including the computed implied volatility based on original df file.
#' @param FP futurePrice.
#' @param K strike price.
#' @param type type of the option whether call option or put option.
#' @param Time time to expiry.
#' @param r the risk free rate, I set it as 0.02 in this package.
#' @param sigma the implied volatility, when it comes to initial test value, I choose the value leading Vage maximum.
#' @param ftol error tolerance.
#' @return Dataframe including the computed implied volatility based on original df file.
#' @example
#' df = data.frame(strike = c(50, 20), type = c("C", "P"), optionPrice = c(1.62,0.01), futurePrice = c(48.03, 48.03), time_to_expiry = c(0.1423, 0.1423))
#' ImpliedVol(df)
#'   strike type optionPrice futurePrice time_to_expiry volatility
#' 1     50    C        1.62       48.03         0.1423  0.3373548
#' 2     20    P        0.01       48.03         0.1423  0.8558107


ImpliedVol <- function(df){
  if (!all(c("strike","type","optionPrice","futurePrice","time_to_expiry") %in% names(df))) {
    stop("ERROR:The datadrame's column names are wrong.")
  }
  blprice <- function(FP, K, type, Time, r, sigma){

    d1 <- (log(FP/ K)+(sigma^2/2)*Time)/(sigma*sqrt(Time))
    d2 <- d1-sigma*sqrt(Time)

    if (type == "C") {
      P <- exp(-r*Time) * (FP*pnorm(d1) - K*pnorm(d2))
      vega <- FP * exp(-r*Time) * dnorm(d1) * sqrt(Time)
    } else if (type == "P") {
      P <- exp(-r*Time) * (K*pnorm(-d2) - FP*pnorm(-d1))
      vega <- FP * exp(-r*Time) * dnorm(d1) * sqrt(Time)
    }
    return(c('theoryprice' = P, 'vega' = vega))
  }
  volatility <- function(FP, K, type, Time, r, optionPrice, ftol = 1e-10) {
    sigma <- sqrt(2 * abs(log(FP/K) + r*Time)/Time)
    TP <- blprice(FP=FP, K=K, Time=Time, r=r, type=type,sigma=sigma)
    while ((abs(TP[1] - optionPrice) > ftol)) {
      sigma <- sigma - (TP[1] - optionPrice) / TP[2]
      TP <- blprice(FP=FP, K=K, Time=Time, r=r, type=type,sigma=sigma)
    }
    sigma
  }
    Vol <- vector(mode = 'numeric', length = nrow(df))
    for (i in seq_len(nrow(df))) {
      Vol[i] <- volatility(FP = df[i, 'futurePrice'],
                           K = df[i, "strike"],
                           type = df[i, 'type'],
                           Time = df[i, 'time_to_expiry'],
                           r = 0.02,
                           optionPrice = df[i, 'optionPrice'])
    }
    return(data.frame(df, volatility = Vol))
}
