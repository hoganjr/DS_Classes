library(tidyverse)
library(dslabs)
library(gtools)

#~~~~~CONFIDENCE INTERVALS~~~~~~
#geom_smooth confidence interval example
data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")

#Monte Carlo sim of confidence intervals
#note that to compute exact 95% confidence interval, we use qnorm(0.975)*SE_hat
p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean

#solving for z with qnorm
z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability for interval

#~~~~~A MONTE CARLO SIMULATION FOR CONFIDENCE INTERVALS~~~~~~~

#note that to compute the exact 95% confidence interval we would use qnorm(0.975)*SE_hat
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

#~~~~~~~POWER~~~~~~~~~~~~~~~~~~~~

#confidence interval for the spread with sample size of 25
#note that for exactly 95% we would use c(-qnorm(0.975),qnorm(0.975)) instead of 2
N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)

#~~~~~P-VALUES~~~~~~~~~~~~~~

#computing a p-value for observed spread of 0.02
N <- 100    # sample size
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))
