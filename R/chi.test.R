
#' @title Calculating Chi Squared p-value
#' @export
#' @return Returns the Chi Squared test statistic.
#' @usage chi.test(x, p = NULL,plot= FALSE)
#' @keywords chi squared test, Chi Statistic
#' @description This function takes as input a numerical vector/obervations \code{x}, and a vector of expected probability \code{p}
#' then calculate the  \code{Chi Squared Statistic} based on these information.
#' @import graphics
#' @import stats
#' @param plot specifies whether or not to make barplot of the observed data and the expectation
#' @param x a numeric vector.
#' @param p a vector of proability, vector of uniform probability if it is unspecified
#' @author Nguyen Khanh Le Ho \cr
#' @examples
#' chi.test(cars$speed, p = NULL)


chi.test <- function(x , p = NULL, plot=FALSE){

  #-----------------------Error Handler--------------------------#

  if(is.list(x)|| !is.vector(x) || !is.numeric(x)){
    stop("'x' must be a numerical vector")
  }
  if(!is.null(p) && length(p) != length(x)){
    stop(" 'x' and 'p' must be of equal length")
  }
  if(!is.null(p) && sum(p)!= 1){
    stop("'p' the probabilities must sum up to 1")
  }
  if(!is.null(p) && length(p[p<0])>0){
    stop("'p' contains negative probabilty")
  }
  if(!is.logical(plot) || length(plot)!= 1){
    stop("'plot' must be either TRUE or FALSE")
  }

  #---------------------------------------------------------------#



  ncat = length(x)
  total = sum(x)
  df = ncat - 1



  if(is.null(p)){
    p <- 1/ncat
    Exp = total*p
    chiT = sum(((x - Exp)^2)/(Exp))
  }else{
    Exp = total*p
    chiT = sum(((x-Exp)^2)/(Exp))
  }

  if(plot== TRUE){
    par(mfrow = c(2,1))
    barplot(x, main = "Observed")
    barplot(Exp, main = "Expected")
  }

  return(chiT)

}
