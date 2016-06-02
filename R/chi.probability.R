
#' @title Calculation the Chi Squared probability.
#' @export
#' @return returns the probability that the \code{Chi} greater than \code{x} (area under the distribution which is greated than \code{x}).
#' @usage chi.probability(x, df = 5, n = 1000, plot = FALSE)
#' @keywords distribution chi Monte Carlo simulation Estimation
#' @description \code{chi.probability} computes the probability which the \code{Chi} of a given degree of freedom is greater than a specified value \code{x}.
#' If \code{x} is a vector, in which case the function will return a vector of probabilities of the components of \code{x}. The \code{plot} option if chosen to
#' be TRUE, then a plot of the density function of the \code{Chi Squared} with degree of freedom \code{df} will be created.
#' @param x specifies the value that should be evaluated in the Chi Square distribution.
#' @param df degrees of freedom in the Chi Square distribution
#' @param n number of random numbers for evaluating the distribution
#' @param plot specifies whether a plot should be created or not
#' @author Nguyen Khanh Le Ho \cr
#' @examples chi.probability(x= 5, df = 5, n = 1000, plot = TRUE)


chi.probability <- function(x, df = 5, n = 1000,  plot = FALSE){

  #----------------------------Error Handler-------------------------------#
  if(!is.numeric(n) || n!= round(n) || n < 1 || length(n)>1){
    stop("'n' must be a natural number")
  }
  if(!is.logical(plot) || length(plot)>1){
    stop("'plot' must be logical non-vector.")
  }
  if(!is.numeric(df) || df!= round(df) || df < 1 || length(df)>1 ){
    stop(" df must be a natural number")
  }
  if(!is.vector(x) ){
    stop(" x must be a numerical vector")
  }
  #---------------------------End Error Handler----------------------------#
  lx <- length(x)

  storage <- matrix(rep(0,lx*n), nrow = lx)
  Pr <- rep(0,length(x))


  for(k in 1:lx){
    for(i in 1:n){
      storage[k,i]<- sum(rnorm(df)^2)
    }
  }

  for(j in 1: lx){
    Pr[j] <- length(storage[j,][storage[j,] > x[j]])/n
  }

  if(plot==TRUE){
    par(mfrow=c(2,1))
    plot(density(storage[1,]))
    plot(ecdf(storage[1,]))
  }
  return(Pr)
}

