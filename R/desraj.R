#' @title Desraj Ordered Estimator
#' @param y vector of vaues of sampled units as per the order of selection
#' @param p vector of selection probabilities as per the order of selection
#' @export
#' @description  Desraj estimator computes the estimated value of a finite population
#'               total when values of the study variable for the sampled units
#'               and the corresponding selection probabilities are given as per the order
#'               of selection. It is meant for use in Probability Proportional to Size
#'               Without Replacement Sampling Scheme.
#'@references  Sampath,S.(2005) Sampling Theory and Methods,Alpha Science International, Ltd
#' @examples
#'  y<-c(16,13,12,10)
#'  p<-c(0.21, 0.34, 0.12,0.10)
#'  desraj(y,p)


desraj<-function(y,p)
{n<-length(y)
t<-vector(mode = "numeric", length = n)
t[1]<-y[1]/p[1];
for ( i in 2:n)
  t[i]<-sum(y[1:(i-1)])+(y[i]*(1-sum(p[1:(i-1)]))/p[i])
rt<-list(est=mean(t),estvar=stats::var(t)/n,tvals=t)
rt
}
