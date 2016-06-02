
#' @title Simulating Buffon's Needles Experiment
#' @export
#' @return An estimation of Pi using the famous buffon's experiment
#' @usage buffon.needle(n=255,l= 2.5, d=3, ind = FALSE, plot= FALSE )
#' @keywords Estimating the pi, buffon experiment
#' @description This function takes as arguments the number of needles \code{n}, the length of needle \code{l} and
#' the distance between the lines \code{d}, then simulate the buffon needle experiment.
#' @details The idea is to generate two random points (x,k) and (xmax, kmax) represents the points which the needle extends.
#' Any point (x,k) on the board is uniformly distributed, by the assumption that it is equally likely to hit any point on the board.
#' At the same time an angle theta is generated from U(0, pi/2). Using this angle the point (xmax,kmax) is generated since xmax = x+l*sin(theta)
#' and kmax = k + l*cos(theta).
#'
#' The board is designed such that there are 5 parallel lines, distance d from each other i.e we have y_1= d, y_2 = 2*d,...,y_5 = 5*d. Every time there is a hit
#' it satisfies that kmax > y_i > k.
#' @import graphics
#' @import stats
#' @param n the number of needles
#' @param l the length of the needles
#' @param d the distance between two lines
#' @param plot specifies whether the board should be drawn.
#' @param ind  specicfies whether or not to give a red color to the needles that hit the lines
#' @author Nguyen Khanh Le Ho \cr
#' @examples
#' buffon.needle(n=2000, l= 2.5 , d= 3 ,  ind = TRUE, plot=TRUE )

buffon.needle<- function(n=255,l=2.5,d=3, ind = FALSE, plot= FALSE){

  theta<-c()
  X<-c()
  K<- c()
  Kmax<-c()
  Xmax<- c()
  hit<- rep(0,n)

  # 1) Draw the board:
  #-------------------
  if(plot){
    Z<- rep(d,(l*20+2))  # Adjusting the board depending on the input of d and l
    Y<- c(-1:(l*20))

    for(i in 1:5){       # Creating 5 lines distance d from each other
      if(i > 1){
        abline(i*d,0, col="blue")
      }else{
        plot(Y,Z, col="blue", type= "l", xlim = c(0,d*5+l), ylim = c(0,d*5), xaxt="n ",ylab = "",xlab = ""  ,yaxt="n", main = "Buffon's Needles Experiment")
        abline(0,0,col="blue")
      }
    }
  }

  # 2) Simulating a random point (X,K) on the board
  #    then using this point to calculate the point
  #    (Xmax, Kmax) which the needle reaches.
  #------------------------------------------------

  for(i in 1:n){
    theta[i]<-runif(1,0,pi/2)
    X[i] <- runif(1,0,d*5)
    K[i] <- runif(1,0,d*5)
    Xmax[i] = X[i]+l*sin(theta[i])
    Kmax[i] = K[i] + l*cos(theta[i])

    # 3) Counting how many needle hit the lines.
    #---------------------------------------
    for(j in 1:5){
      if((K[i]<j*d) && (Kmax[i]>j*d)){
        hit[i]=1
      }
    }
    # 4) Draw the needles:
    #------------------
    if(ind && plot){
      segments(X[i],K[i],Xmax[i],Kmax[i], col=hit[i]+1)
    }else if(plot){
      segments(X[i],K[i],Xmax[i],Kmax[i])
    }

  }
  EstP =(2*l*n)/(sum(hit)*d)
  return(EstP )
}
