alpha_f.test_solve <- function(x, df1, df2, evidence, paired){
  (evidence - bf_bic(x, df1, df2, paired))^2
}

alpha_t.test_solve <- function(x, n1, n2, evidence, rscale, one.sided){
  (evidence - bf_t.test(x, n1, n2, rscale, one.sided))^2
}

alpha_sample_solve <- function(i, power_function, errorgoal, costT1T2, priorH1H0, error){

  res <- optimal_alpha(power_function = paste(stringr::str_replace(power_function, "sample_n", as.character(i))), costT1T2 = costT1T2, priorH1H0 = priorH1H0, error = error)
  (errorgoal - res$errorrate)^2
}

bf_t.test <- function(t, n1, n2 = 0, rscale = sqrt(2)/2, one.sided = FALSE){
  # if (n2 == 0) {
  #   v <- n1 -1
  #   N <- n1
  # } else {
  #   N <- n1*n2/(n1+n2)
  #   v <- n1 + n2 -2
  # }
  # if (Cauchy == FALSE) {
  #   ml1 <- 1/sqrt(1+N)*(1+t^2/((1+N)*v))^(-(v+1)/2)
  #   ml0 <- (1+t^2/v)^(-(v+1)/2)
  #   return(ml1/ml0)
  # } else {
  if(!one.sided){
    return(exp(BayesFactor::ttest.tstat(t, n1, n2, rscale = rscale)$bf))
  }
  else if(one.sided){
    return(exp(BayesFactor::ttest.tstat(t, n1, n2, rscale = rscale, nullInterval = c(0, Inf))$bf))
  }
}

bf_bic <- function(Fval, df1, df2, repeated=FALSE, report.as="BF10") {
  if (repeated==FALSE) {
    N = df1+df2+1
  }
  else {
    N = df1+df2
  }

  bf = sqrt(N^df1*(1+Fval*df1/df2)^(-1*N))

  if (report.as=="BF01"){
    return(c(B01=bf))
  }
  else {
    return(c(B10=1/bf))
  }
}

