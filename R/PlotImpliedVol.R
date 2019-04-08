#' Use black76 model to compute the implied volatility of a set of European options on future contracts and plot them.
#' This function - plotImpliedVol will return plots based on computated implied volatility.
#' @param FP futurePrice.
#' @param K strike price.
#' @param type type of the option whether call option or put option.
#' @param Time time to expiry.
#' @param r the risk free rate, I set it as 0.02 in this package.
#' @param sigma the implied volatility, when it comes to initial test value, I choose the value leading Vage maximum.
#' @param ftol error tolerance.
#' @return Graph - "Implied Volatility & Time to Expiry" and "Implied Volatility & Option Price".
#' @example
#' df = data.frame(strike = c(50, 20), type = c("C", "P"), optionPrice = c(1.62,0.01), futurePrice = c(48.03, 48.03), time_to_expiry = c(0.1423, 0.1423))
#' plotImpliedVol(df)


plotImpliedVol <- function(df){
  library(ggplot2)
  library(gridExtra)
  df0=ImpliedVol(df)
  b<-ggplot(df0, aes(x=volatility, y=time_to_expiry, colour = type))+geom_point()+ggtitle("Implied Volatility & Time to Expiry")+xlab('Implied Volatility')+ylab('Time to Expiry')+scale_colour_hue('Option Type',breaks=c("C","P"),labels=c("Call Option","Put Option"))+theme(plot.title = element_text(hjust = 0.5))
  c<-ggplot(df0, aes(x=volatility, y=optionPrice, colour = type))+ggtitle("Implied Volatility & Option Price")+geom_point()+xlab('Implied Volatility')+ylab('Option Price')+scale_colour_hue('Option Type',breaks=c("C","P"),labels=c("Call Option","Put Option"))+theme(plot.title = element_text(hjust = 0.5))
  grid.arrange(b,c)
}
