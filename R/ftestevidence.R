#' Justify your alpha level by avoiding the Lindley paradox or aiming for moderate or strong evidence when using anova.
#' @param evidence Desired level of evidence: "Lindley" to avoid the Lindley Paradox, "moderate" to achieve moderate evidence and "strong" to achieve strong evidence.
#' Users that are more familiar with Bayesian statistics can also directly enter their desired Bayes factor.
#' @param df1 Numerator degrees of freedom.
#' @param df2 Denominator degrees of freedom.
#' @param paired If true a within subjects design is assumed.
#' @param printplot If true prints a plot relating Bayes factors and p-values.
#' @return alpha level required for a two-sample t-test.
#' @examples
#' ## Avoid the Lindley paradox for an anova with 1 numerator and 248 denominator degrees of freedom.
#' ftestEvidence("lindley", 1, 248)
#' @section References:
#' Maier & Lakens (2021). Justify Your Alpha: A Primer on Two Practical Approaches
#' @export
#' @importFrom stats optim pf pt
#' @importFrom grDevices recordPlot
#' @importFrom graphics abline axis points

ftestEvidence <- function(evidence, df1, df2, paired = FALSE, printplot = FALSE){

  if (evidence == "lindley"){
    evidence = 1
  }
  if (evidence == "moderate"){
    evidence = 3
  }
  if (evidence == "strong"){
    evidence = 10
  }

  ##optim to find alpha level for evidence
  crit_f <- optim(5, alpha_f.test_solve, lower = 0.01, upper = 100, method = "L-BFGS-B", control = list(maxit = 100000),
                  df1 = df1, df2 = df2, evidence = evidence, paired = paired)$par
  alpha <- (1 - pf(crit_f, df1, df2))

  #make plot and store in output
  if(printplot){

    lindley  <- ftestEvidence(1, df1, df2, paired, printplot = F)[[1]]
    moderate <- ftestEvidence(3, df1, df2, paired, printplot = F)[[1]]
    strong   <- ftestEvidence(10, df1, df2, paired, printplot = F)[[1]]
    indicated <- ftestEvidence(evidence, df1, df2, paired, printplot = F)[[1]]

    loops <- seq(from = 0, to = 100, by = 0.01)
    p <- numeric(length(loops))
    bf <- numeric(length(loops))
    fval <- numeric(length(loops))
    i <- 0
    for(f in loops){
      i <- i+1
      bf[i] <- bf_bic(f, df1, df2, paired)
      p[i] <- (1 - pf(f, df1, df2))
      fval[i] <- f
    }
    plot(p, bf, type="l", lty=1, lwd=3, xlim = c(0, max(c(0.05, lindley))), ylim = c(0.1, 10), axes = F, xlab = "p-value", ylab = "Bayes factor", log = "y")
    axis(side=1, at = c(0, as.numeric(lindley), as.numeric(moderate), as.numeric(strong), 0.05, indicated), labels = c(0, round(lindley, digits = 3), round(moderate, digits = 3), round(strong, digits = 3), 0.05, round(indicated, digits = 3)),  lwd = 3, las = 3)
    axis(side=2, at = c(0.1, 0.33, 1, 3, 10), labels = c("1/10", "1/3", 1, 3, 10), lwd = 3)
    points(indicated, evidence, col = "red", lwd = 4)
    abline(h = c(0.1, 0.33, 1, 3, 10), col = "gray", lty = 2)
    abline(v = c(lindley, moderate, strong), lty = 3)
    abline(v = indicated, lty = 3, col = "red")
    plot <- recordPlot()
    }
  if(printplot){
      return(list(alpha = alpha,
              evidence = evidence,
              plot = plot))
  } else {
      return(list(alpha = alpha,
              evidence = evidence))
  }
}

