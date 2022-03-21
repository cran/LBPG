#' The probability density function of the length-biased power Garima distribution.
#'
#' The LBPG package computes the probability density, the cumulative density distribution and the quantile function of the length-biased power Garima (LBPG) distribution, and generates sample values with random variables that has the LBPG distribution.
#'
#' @param x vector of positive quantile.
#' @param lambda positive parameter(Transformed parameter).
#' @param beta positive parameter(Shape parameter).
#' @references Kittipong Klinjan and Sirinapa Aryuyuen(2021), The length-biased power Garima distribution and its application to model lifetime data,Songklanakarin Lournal of Science asd Techno;ogy (SJST),Volume 43 No.3(May - Jun. 2021),pp667-676, <DOI: 10.14456/sjst-psu.2021.89>
#' @return dLBPG gives the probability density function.
#' @export
#'
#' @examples
#' dLBPG(5.7,1.5,2.5)
dLBPG<-function(x,lambda,beta){
  a1<-1+1/lambda; a2<-2+1/lambda;
  p1<-lambda*(beta^a1)
  p2<-(a2+beta)*gamma(a1)
  p3<-(1+beta+(beta*(x^lambda)))*(x^lambda)*exp(-beta*x^lambda)
  ff<-(p1/p2)*p3
  return(ff)
}
