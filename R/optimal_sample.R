#' Justify your alpha level by minimizing or balancing Type 1 and Type 2 error rates.
#' @param power_function Function that outputs the power, calculated with an analytic function.
#' @param errorgoal Desired weighted combined error rate
#' @param costT1T2 Relative cost of Type 1 errors vs. Type 2 errors.
#' @param priorH1H0 How much more likely a-priori is H1 than H0?
#' @param error Either "minimize" to minimize error rates, or "balance" to balance error rates.
#' @param printplot Print a plot to illustrate the alpha level calculation. This will make the function considerably slower.
#' @return
#' alpha = alpha or Type 1 error that minimizes or balances combined error rates,
#' beta = beta or Type 2 error that minimizes or balances combined error rates,
#' errorrate = weighted combined error rate,
#' objective = value that is the result of the minimization, either 0 (for balance) or the combined weighted error rates,
#' samplesize = the desired samplesize.
#'
#' @examples
#' ## Optimize power for a independent t-test, smallest effect of interest
#' ## d = 0.5, desired weighted combined error rate = 5%
#' \dontrun{
#' res <- optimal_sample(power_function = "pwr::pwr.t.test(d = 0.5, n = sample_n, sig.level = x,
#' type = 'two.sample', alternative = 'two.sided')$power",errorgoal = 0.05)
#' res$alpha
#' res$beta
#' res$errorrate
#' res$samplesize
#' }
#' @section References:
#' Maier & Lakens (2021). Justify Your Alpha: A Primer on Two Practical Approaches
#' @importFrom stats optimize
#' @import qpdf
#' @export
#'
#'

optimal_sample <- function(power_function, errorgoal = 0.05, costT1T2 = 1, priorH1H0 = 1, error = "minimize", printplot = FALSE) {


  samplesize<- optim(20, alpha_sample_solve, lower = 2, upper = 100000, method = "L-BFGS-B",
                     power_function = power_function, errorgoal = errorgoal, costT1T2 = costT1T2, priorH1H0 = priorH1H0, error = error)$par
  samplesize <- ceiling(samplesize)
  result <- optimal_alpha(power_function = paste(stringr::str_replace(power_function, "sample_n", as.character(samplesize))), costT1T2 = costT1T2, priorH1H0 = priorH1H0, error = error)
  if(printplot){
    alphas <- c()
    betas <-c()
    Error <-c()
    Samplesize<- c()

    for (i in seq(10, (round(samplesize, digits = -1) + 20), 5)) {
    res <- optimal_alpha(power_function = paste(stringr::str_replace(power_function, "sample_n", as.character(i))), costT1T2 = costT1T2, priorH1H0 = priorH1H0, error = error)
    alphas <- c(alphas, res$alpha)
    betas <- c(betas, res$beta)
    Error <- c(Error, res$errorrate)
    Samplesize <- c(Samplesize, i)
    #print(paste("Progress: ", round((i/(samplesize + 20))*100, digits =0), "%", sep = ""))
    }

    defaultW <- getOption("warn")

    options(warn = -1)
    line <- rep(0.05, length(Samplesize))
    dataplot <- as.data.frame(cbind(alphas, betas, Error, Samplesize, line))

    colors <- c("Beta" = "grey", "Weighted Combined Error Rate of 5%" = "red", "Weighted Combined Error Rate" = "black", "Alpha" = "grey")
    linetypes <- c("Beta" = "dotted", "Weighted Combined Error Rate of 5%" = "solid", "Weighted Combined Error Rate" = "solid", "Alpha" = "dashed")

    plot <- ggplot2::ggplot(ggplot2::aes(x=Samplesize), data=dataplot) +
      ggplot2::geom_line(size = 1.3, ggplot2::aes(y = Error, color = "Weighted Combined Error Rate", linetype = "Weighted Combined Error Rate")) +
      ggplot2::theme_minimal(base_size = 18) +
      ggplot2::scale_x_continuous("Sample Size", seq(10,150,20)) +
      ggplot2::scale_y_continuous("",seq(0,1,0.05), limits = c(0,0.5)) +
      ggplot2::geom_line(ggplot2::aes(y=alphas,  color = "Alpha", linetype = "Alpha"), data=dataplot, size = 1.3) +
      ggplot2::geom_line(ggplot2::aes(y=betas,  color = "Beta", linetype = "Beta"), data=dataplot, size = 1.3) +
      ggplot2::geom_point(ggplot2::aes(x = samplesize, y = (costT1T2 * result$alpha + priorH1H0 * result$beta) / (costT1T2 + priorH1H0), color = "Weighted Combined Error Rate of 5%", linetype = "Weighted Combined Error Rate of 5%"), size = 3) +
      ggplot2::labs(x = "Sample Size",
           y = "",
           color = "",
           linetype = "") +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_linetype_manual(values = linetypes) +
      ggplot2::theme(legend.position="bottom")

    plot <- plot +  ggplot2::geom_segment(ggplot2::aes(x = samplesize, xend = samplesize, y = (costT1T2 * result$alpha + priorH1H0 * result$beta) / (costT1T2 + priorH1H0), yend = -Inf), color = "red", linetype = "dotted")
    print(plot)
    options(warn = defaultW)


  }

  if(printplot){
  list(alpha = result$alpha,
                 beta = result$beta,
                 errorrate = (costT1T2 * result$alpha + priorH1H0 * result$beta) / (costT1T2 + priorH1H0),
                 objective = result$objective,
                 samplesize = samplesize,
                plot = plot
  )
  } else {
    list(alpha = result$alpha,
         beta = result$beta,
         errorrate = (costT1T2 * result$alpha + priorH1H0 * result$beta) / (costT1T2 + priorH1H0),
         objective = result$objective,
         samplesize = samplesize
         )
  }
}
