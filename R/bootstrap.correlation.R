
#' @title Bootstrap Estimation of Correlation
#' @export
#' @return Returns a list containing: the estimated correlation of the two given vectors, the bootstrap estimated correlation of the data and the 95 percent confidence interval.
#' @usage bootstrap.correlation(n=2000, x , y, plot = FALSE)
#' @keywords Estimating the correlation, bootstrap estimate, and confidence interval
#' @description This function takes as arguments the number of bootstrap replicates \code{n},
#' \code{x} and \code{y} which are two numerical vectors / data, then bootstrap from \code{x ,y}, and calculate the correlation of these data for each resampling n times.
#' If \code{plot} is chosen to be TRUE, then the function plots the histogram, as well as the 95% confidence interval for the
#' estimates.
#' @import graphics
#' @import stats
#' @param x a numeric vector.
#' @param y a numeric vector.
#' @param n number of bootstrap samples.
#' @param plot specifies whether a plot should be created or not
#' @author Nguyen Khanh Le Ho \cr
#' @examples
#' bootstrap.correlation(n=2000,cars$speed,cars$dist, plot=TRUE )


bootstrap.correlation <- function(n = 2000, x , y, plot = FALSE){

  #------------------------------------Error Handler:--------------------------------------#
  # ---------------------------------------------------------------------------------------#
  # 'n' must be numeric, natural number greater than 1.                                    #
  # 'x, y' must be numerical vectors.                                                      #
  # 'plot' must booblean valued.                                                           #
  #----------------------------------------------------------------------------------------#

  if(!is.numeric(n) || n!= round(n) || n < 1 ){
    stop("The number of bootstrap sample 'n' must be a natural number greater than 1.")
  }
  if(is.list(y) || is.list(x) || !is.numeric(x) || !is.numeric(y) || is.matrix(x+y) || !is.vector(x+y)   ){
    stop(" The given data 'x' or 'y' must be a numercial vector.")
  }
  if(length(x)!= length(y) ){
    stop("Length of the given data x and y must be equal")
  }
  if(!is.logical(plot)){
    stop("'plot' must be logical.")
  }

  #---------------------------------------End Error Handler---------------------------------#

  storage <- c(1:n)
  size <- length(x)

  #Calculation the populations correlation:
  #---------------------------------------
  estCor <- cor(x,y)

  #Bootstrap estimation of correlation and confidence interval:
  #-----------------------------------------------------------


  for(i in 1:n){
    k<-  sample(c(1:size),size,replace = TRUE) # sampling from the population
    storage[i]<-  cor(x[k], y[k])
  }

  bootCor <- mean(storage)           # Calculating the bootstrap est. of correlation
  bias <- mean(storage - estCor)     # Bias
  SE = sd(storage)                   # Stanard Error
  CI = c(bootCor - 1.96*SE, bootCor+1.96*SE)  # 95% Confidence interval

  if(plot== TRUE){
    hist(storage, main = "Blue line: Estimated, Red lines: ConfInt", xlab = "correlation")
    abline(v = c(CI[1], CI[2], estCor), col=c("red","red", "blue"))
  }


  f <- list( "Estimated" = estCor, "BootstrapEst" = bootCor ,"Bias" = bias, "ConfInt" = CI)

  return(f)

}
