#' The cumulative density function of the length-biased power Garima distribution.
#'
#'The LBPG package computes the probability density, the cumulative density distribution and the quantile function of the length-biased power Garima (LBPG) distribution, and generates sample values with random variables that has the LBPG distribution.
#'
#' @param x vector of positive quantile.
#' @param lambda positive parameters(Transformed parameter).
#' @param beta positive parameters(Shape parameter).
#' @references Kittipong Klinjan and Sirinapa Aryuyuen(2021), The length-biased power Garima distribution and its application to model lifetime data,Songklanakarin Lournal of Science asd Techno;ogy (SJST),Volume 43 No.3(May - Jun. 2021),pp667-676, <DOI: 10.14456/sjst-psu.2021.89>
#' @import gsl
#' @return pLBPG gives the cumulative density function.
#' @export
#'
#'
#' @examples
#' pLBPG(0.5,1.5,2.5)
pLBPG<-function(x,lambda,beta){
  l1<-1+1/lambda;
  l2<-2+1/lambda;
  b<-beta*(x^lambda);
  lb<-2+(1/lambda)+beta
  Fx1<-(1+beta)*(gamma_inc(l1,b))+(gamma_inc(l2,b))
  Fx2<-lb*gamma(l1)
  Fx<-1-(Fx1/Fx2)
  return(Fx)
}
