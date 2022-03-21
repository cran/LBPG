#' The quantile function of the length-biased power Garima distribution.
#'
#' The LBPG package computes the probability density, the cumulative density distribution and the quantile function of the length-biased power Garima (LBPG) distribution, and generates sample values with random variables that has the LBPG distribution.
#'
#' @param p  vector pf probabilities.
#' @param alpha positive parameters(Transformed parameter).
#' @param beta positive parameters(Shape parameter).
#' @import gsl
#' @references Kittipong Klinjan and Sirinapa Aryuyuen(2021), The length-biased power Garima distribution and its application to model lifetime data,Songklanakarin Lournal of Science asd Techno;ogy (SJST),Volume 43 No.3(May - Jun. 2021),pp667-676, <DOI: 10.14456/sjst-psu.2021.89>
#' @return qLBPG gives the quantile function.
#' @export
#'
#' @examples
#' qLBPG(0.5,1.5,2.5)
qLBPG <- function(p,alpha,beta){
  n <- length(p);
  x <- numeric(n);
  for (i in 1:n){
    k <- 0;
    if( p[i] >= pLBPG(k,alpha,beta) ){
      while ( p[i] >= pLBPG(k,alpha,beta) ) #cdf of LBPG
      {
        k <- k+0.001
      }
    }
    x[i] <- k
  }
  return(x)
}
