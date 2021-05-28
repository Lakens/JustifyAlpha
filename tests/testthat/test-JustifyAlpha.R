library("JustifyAlpha")

context("Test Lindley Paradox approaches")

 test_that("check whether Bayes Factors are calculated correctly for anova",{
   df1 <- c(1:16)
   df2 <- seq(1, 160, 10)
   for (k in 1:10)
   for (i in 1:length(df1)){
     alpha <- ftestEvidence(k, df1[i], df2[i])[[1]]
     expect_true(k == round(bf_bic(qf(1-alpha, df1[i], df2[i]), df1[i], df2[i]), digits = 2))
   }


 })

 test_that("check whether Bayes Factors are calculated correctly for t-tests",{

   n1 <- c(seq(10, 50, 10))
   n2 <- c(seq(10, 50, 10))
   for (k in c(1,3,10)){
     for (i in 1:length(n1)){
       k = 1
       i = 3
       alpha <- ttestEvidence(k, n1[i], n2[i])[[1]]
       expect_equal(k,round(exp(BayesFactor::ttest.tstat(qt((1-alpha/2), n1[i]+n2[i]-2),n1[i],n2[i])$bf), digits = 2))

     alpha <- ttestEvidence(k, n1[i], n2[i])[[1]]
       expect_equal(k, round(bf_t.test(qt((1-alpha/2), n1[i]+n2[i]-2), n1[i], n2[i]), digits = 2))
     }
   }
 })

context("Test Balancing or minimizing error rates")

test_that("check whether calculated error rates match actual ones",{

  n1 <- c(seq(10, 50, 10))
  power_function <- "pwr::pwr.t.test(d = 0.5, n = 50, sig.level = x, type = 'two.sample', alternative = 'two.sided')$power"
  for (k in c(1,3,10)){
    for (i in n1){
      res <- optimal_alpha(power_function = paste(stringr::str_replace(power_function, "n = i", paste("n =", i))))
      expect_true(res$errorrate == (1 * res$alpha + 1 * res$beta) / (1 + 1))
    }

  for (odds in c(1, 5, 10)){
    for(cost in c(1, 3, 10)){
    for (i in n1){
      res <- optimal_alpha(power_function = paste(stringr::str_replace(power_function, "n = i", paste("n =", i))), priorH1H0 = odds, costT1T2 = cost)
      expect_true(res$errorrate == (cost * res$alpha + odds * res$beta) / (odds+ cost))
    }
    }
  }

  }
})
# 
test_that("Test whether power is calculated correctly",{

  n1 <- c(seq(10, 50, 10))
  power_function <- "pwr::pwr.t.test(d = 0.5, n = 50, sig.level = x, type = 'two.sample', alternative = 'two.sided')$power"
  for (k in c(1,3,10)){
    for (i in n1){
      res <- optimal_alpha(power_function = paste(stringr::str_replace(power_function, "n = i", paste("n =", i))))
      expect_true((1-res$beta) == pwr::pwr.t.test(d = 0.5, n = 50, sig.level = res$alpha, type = 'two.sample', alternative = 'two.sided')$power)
    }

    for (odds in c(1, 5, 10)){
      for(cost in c(1, 3, 10)){
        for (i in n1){
          res <- optimal_alpha(power_function = paste(stringr::str_replace(power_function, "n = i", paste("n =", i))), priorH1H0 = odds, costT1T2 = cost)
          expect_true((1-res$beta) == pwr::pwr.t.test(d = 0.5, n = 50, sig.level = res$alpha, type = 'two.sample', alternative = 'two.sided')$power)
        }
      }
    }

  }
})
# 
# #only one test otherwise too slow
test_that("Alpha_Sample works correctly",{
  res <- optimal_sample("pwr::pwr.t.test(d = 0.5, n = sample_n, sig.level = x, type = 'two.sample', alternative = 'two.sided')$power", costT1T2 = 3, priorH1H0 = 3)
  expect_true((1-res$beta) == pwr::pwr.t.test(d = 0.5, n = res$samplesize, sig.level = res$alpha, type = 'two.sample', alternative = 'two.sided')$power)
})

# test_that("Shiny App can be run",{
#     runApp()
# })
  