% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/minimize_balance_alpha.R
\name{optimal_alpha}
\alias{optimal_alpha}
\title{Justify your alpha level by minimizing or balancing Type 1 and Type 2 error rates.}
\usage{
optimal_alpha(
  power_function,
  costT1T2 = 1,
  priorH1H0 = 1,
  error = "minimize",
  verbose = FALSE,
  printplot = FALSE
)
}
\arguments{
\item{power_function}{Function that outputs the power, calculated with an analytic function.}

\item{costT1T2}{Relative cost of Type 1 errors vs. Type 2 errors.}

\item{priorH1H0}{How much more likely a-priori is H1 than H0?}

\item{error}{Either "minimize" to minimize error rates, or "balance" to balance error rates.}

\item{verbose}{Print each iteration of the optimization function if TRUE. Defaults to FALSE.}

\item{printplot}{Print a plot to illustrate the alpha level calculation.}
}
\value{
Returns a list of the following
alpha = alpha or Type 1 error that minimizes or balances combined error rates,
beta = beta or Type 2 error that minimizes or balances combined error rates,
errorrate = weighted combined error rate,
objective = value that is the result of the minimization, either 0 (for balance) or the combined weighted error rates.
plot_data = data used for plotting (only if printplot = TRUE)
plot = plot of error rates depending on alpha (only if printplot = TRUE)
}
\description{
Justify your alpha level by minimizing or balancing Type 1 and Type 2 error rates.
}
\section{References}{

Maier & Lakens (2021). Justify Your Alpha: A Primer on Two Practical Approaches
}

\examples{
## Optimize power for a independent t-test, smallest effect of interest
## d = 0.5, 100 participants per condition
res <- optimal_alpha(power_function = "pwr::pwr.t.test(d = 0.5, n = 100,
sig.level = x, type = 'two.sample', alternative = 'two.sided')$power")
res$alpha
res$beta
res$errorrate
}
