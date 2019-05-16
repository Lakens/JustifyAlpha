#' Justify your alpha level by minimizing or balancing Type 1 and Type 2 error rates.
#' @param power_function Function that outputs the power, calculated with an analytic function.
#' @param costT1T2 Relative cost of Type 1 errors vs. Type 2 errors.
#' @param prior_H1H0 How much more likely a-priori is H1 than H0?
#' @param error Either "minimal" to minimize error rates, or "balance" to balance error rates.
#' @param verbose Print each iteration of the optimization function if TRUE. Defaults to FALSE.
#' @return
#' alpha = alpha or Type 1 error that minimizes or balances combined error rates
#' beta = beta or Type 2 error that minimizes or balances combined error rates
#' objective = value that is the result of the minimization, either 0 (for balance) or the combined weighted error rates
#'
#' @examples
#' ## Optimize power for a independent t-test, smallest effect of interest
#' ## d = 0.5, 100 participants per condition
#' res <- optimal_alpha(power_function = "pwr::pwr.t.test(d = 0.5, n = 100,
#' sig.level = x, type = 'two.sample', alternative = 'two.sided')$power")
#' res$alpha
#' res$beta
#' @section References:
#' too be added
#' @importFrom stats optimize
#' @export
#'
optimal_alpha <- function(power_function, costT1T2 = 1, prior_H1H0 = 1, error = "minimal", verbose = FALSE) {
  #Define the function to be minimized
  f = function(x, power_function, costT1T2 = 1, prior_H1H0 = 1, error = "minimal") {
    y <- 1 - eval(parse(text=paste(power_function)))
    if(verbose == TRUE){
      print(c(x, y, x+y)) #optional: print alpha, beta, and objective
    }
    if(error == "balance"){
      max((costT1T2*x - prior_H1H0*y)/(prior_H1H0+1), (prior_H1H0*y - costT1T2*x)/(prior_H1H0+1))
    } else if (error == "minimal"){
      2*(costT1T2*x + prior_H1H0*y)/(prior_H1H0+1)
    }
  }
  #Run optimize to find the minimum
  res <- stats::optimize(f,
                  c(0, 1),
                  tol = 0.00001,
                  power_function = power_function,
                  costT1T2 = costT1T2,
                  prior_H1H0 = prior_H1H0,
                  error = error)
  if(error == "balance"){
    beta <- res$minimum - res$objective
  } else if (error == "minimal"){
    beta <- res$objective - res$minimum
  }
  x <- res$minimum
  #Store results
  invisible(list(alpha = res$minimum,
                 beta = 1 - eval(parse(text=paste(power_function))),
                 objective = res$objective
  )
  )
}
