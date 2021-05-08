## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
#if(!require(justifieR)){devtools::install_github("Lakens/justifieR")}
library(justifieR)
library(pwr)
library(ggplot2)


## ----minimal, fig.width = 6---------------------------------------------------
res1 <- optimal_alpha(power_function = "pwr.t.test(d=0.5, n=64, sig.level = x, type='two.sample', alternative='two.sided')$power",
              error = "minimal",
              costT1T2 = 1,
              priorH1H0 = 1,
              verbose = FALSE, 
              printplot = TRUE)

res1$alpha
res1$beta
res1$errorrate

## ----mudgeplot, fig.width = 6-------------------------------------------------
resplot1 <- optimal_alpha(power_function = "pwr::pwr.t.test(d = 1, n = 3, sig.level = x, type = 'two.sample', alternative = 'two.sided')$power", error = "minimal", costT1T2 = 1, printplot = FALSE)
resplot2 <- optimal_alpha(power_function = "pwr::pwr.t.test(d = 1, n = 10, sig.level = x, type = 'two.sample', alternative = 'two.sided')$power", error = "minimal", costT1T2 = 1, printplot = FALSE)
resplot3 <- optimal_alpha(power_function = "pwr::pwr.t.test(d = 1, n = 30, sig.level = x, type = 'two.sample', alternative = 'two.sided')$power", error = "minimal", costT1T2 = 1, printplot = FALSE)

plot_data <- rbind(resplot1$plot_data, resplot2$plot_data, resplot3$plot_data)
plot_data$n <- as.factor(rep(c(3, 10, 30), each = 9999))

w_c_alpha_plot <- ggplot(data=plot_data, aes(x=alpha_list, y=w_c_list)) +
  geom_line(size = 1.3, aes(linetype = n)) +
  geom_point(aes(x = resplot1$alpha, y = (1 * resplot1$alpha + 1 * (resplot1$beta)) / (1 + 1)), color="red", size = 3) +
  geom_point(aes(x = resplot2$alpha, y = (1 * resplot2$alpha + 1 * (resplot2$beta)) / (1 + 1)), color="red", size = 3) +
  geom_point(aes(x = resplot3$alpha, y = (1 * resplot3$alpha + 1 * (resplot3$beta)) / (1 + 1)), color="red", size = 3) +
  theme_minimal(base_size = 16) +
  scale_x_continuous("alpha", seq(0,1,0.1)) +
  scale_y_continuous("weighted combined error rate", seq(0,1,0.1), limits = c(0,1))
w_c_alpha_plot

## ----balance, fig.width = 6---------------------------------------------------
res2 <- optimal_alpha(power_function = "pwr.t.test(d=0.5, n=64, sig.level = x, type='two.sample', alternative='two.sided')$power",
              error = "balance",
              costT1T2 = 1,
              priorH1H0 = 1,
              verbose = FALSE, 
              printplot = TRUE)

res2$alpha
res2$beta
res2$errorrate

## ----cost minimal, fig.width = 6----------------------------------------------
res3 <- optimal_alpha(power_function = "pwr.t.test(d=0.5, n=64, sig.level = x, type='two.sample', alternative='two.sided')$power",
              error = "minimal",
              costT1T2 = 4,
              priorH1H0 = 1,
              verbose = FALSE, 
              printplot = TRUE)

res3$alpha
res3$beta
res3$errorrate

## ----cost balance, fig.width = 6----------------------------------------------
res4 <- optimal_alpha(power_function = "pwr.t.test(d=0.5, n=64, sig.level = x, type='two.sample', alternative='two.sided')$power",
              error = "balance",
              costT1T2 = 4,
              priorH1H0 = 1,
              verbose = FALSE, 
              printplot = TRUE)

res4$alpha
res4$beta
res4$errorrate

## ----prior, fig.width = 6-----------------------------------------------------
res5 <- optimal_alpha(power_function = "pwr.t.test(d=0.5, n=64, sig.level = x, type='two.sample', alternative='two.sided')$power",
              error = "minimal",
              costT1T2 = 1,
              priorH1H0 = 4,
              verbose = FALSE, 
              printplot = TRUE)

res5$alpha
res5$beta
res5$errorrate

## ----optimal sample-----------------------------------------------------------
res6 <- optimal_sample(power_function = "pwr.t.test(d=0.5, n = i, sig.level = x, type='two.sample', alternative='two.sided')$power", 
                       error = "minimal", 
                       errorgoal = 0.05, 
                       costT1T2 = 1,
                       priorH1H0 = 1)

res6

