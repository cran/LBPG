#' Random number generating of the length-biased power Garima distribution
#'
#' The LBPG package computes the probability density, the cumulative density distribution and the quantile function of the length-biased power Garima (LBPG) distribution, and generates sample values with random variables that has the LBPG distribution.
#'
#' @param n number of observations.If length(n)>1,the length is taken no be the number required.
#' @param alpha positive parameters(Transformed parameter).
#' @param beta positive parameters(Shape parameter).
#' @references Kittipong Klinjan and Sirinapa Aryuyuen(2021), The length-biased power Garima distribution and its application to model lifetime data,Songklanakarin Lournal of Science asd Techno;ogy (SJST),Volume 43 No.3(May - Jun. 2021),pp667-676, <DOI: 10.14456/sjst-psu.2021.89>
#' @return rLBPG generates sample values of random variables.
#' @export
#' @import stats gsl
#' @examples
#' rLBPG(5,1.5,1)
rLBPG <- function(n,alpha,beta){
  x<-numeric();
  u<-runif(n);
  x<-qLBPG(u,alpha,beta);
  return(x)
}
