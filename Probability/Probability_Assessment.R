#ASSESSMENT: THE BIG SHORT
library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)
str(death_prob)

#~~~~~~QUESTIONS 1 & 2: INSURANCE RATES, PART 1~~~~~~~~
payout <- -150000
prem <- 1150
n <- 1000

#1a
#death prob of a 50 y.o. female
p <- death_prob %>% filter(sex == "Female" & age == 50) %>% pull(prob)

#1b
#expected value of the net profit on one policy for a 50 yo female
(p*payout + (1-p)*prem)

#1c
#standard error of the profit 
abs(payout-prem)*sqrt(p*(1-p))

#1d
#expected value of the profit over all 1,000 policies for 50 yo females
n*(p*payout + (1-p)*prem)

#1e
#standard error of the sum of expected profit for all 1,000 policies on 50 yo females
sqrt(n)*abs(payout-prem)*sqrt(p*(1-p))

#1f
#use CLT to calculate prob that the company loses money on this set of 1000 policies
pnorm(0, n*(p*payout + (1-p)*prem), sqrt(n)*abs(payout-prem)*sqrt(p*(1-p)))

#2a
#prob of deat of 50 yo male
p_male <- death_prob %>% filter(sex == "Male" & age == 50) %>% pull(prob)

#2b
#what premium should be charged to get an expected male profit of $700k
prem_male <- (700000/n - payout*p_male)/(1-p_male)
prem_male

#2c
#with new prem rate calculate std error for 1000 prem
sqrt(n)*abs(payout-prem_male)*sqrt(p_male*(1-p_male))

#2d
#prob of losing money for 1k policies of 50 yo males
pnorm(0, 700000, sqrt(n)*abs(payout-prem_male)*sqrt(p_male*(1-p_male)))


#~~~~~~~~QUESTIONS 3 & 4: INSURANCE RATES, PART 2~~~~~~~~~

#a pandemic changes the prob of death within 1 year for 50 yo to 0.015
payout <- -150000
prem <- 1150
n <- 1000
p_pand <- 0.015

#3a
#expected value for profits on 1k policies
exp_val_pand <- n*(p_pand*payout + (1-p_pand)*prem)

#3b
#std error on the 1k policies
std_err_pand <- sqrt(n)*abs(payout-prem)*sqrt(p_pand*(1-p_pand))

#3c
#probability of losing money
pnorm(0, exp_val_pand, std_err_pand)

#3d
#probability of losing $1M
pnorm(-10^6, exp_val_pand, std_err_pand)

#3e
#given seq p, what's the lowest death prob for which chances of losing money exceeds 90%
p_seq <- seq(0.01, 0.03, 0.001)
losses <- function(N){
  deaths <- pnorm(0, n*(N*payout + (1-N)*prem), sqrt(n)*abs(payout-prem)*sqrt(N*(1-N)))
  deaths > 0.9
}
data.frame(p_seq, sapply(p_seq,losses))

#3f
#given new seq, what's the lowest death prob for which losing $1M exceeds 90%
p_seq2 <- seq(0.01, 0.03, 0.0025)
losses <- function(N){
  deaths <- pnorm(-10^6, n*(N*payout + (1-N)*prem), sqrt(n)*abs(payout-prem)*sqrt(N*(1-N)))
  deaths > 0.9
}
data.frame(p_seq2, sapply(p_seq2,losses))

#4a
set.seed(25, sample.kind = "Rounding")
#define sampling model w 1k loans and p_loss = 0.015, -$150k claims, $1150 profit
p_loss = 0.015
cases <- sample( c(1150,-150000), n, prob=c(1-p_loss, p_loss), replace = TRUE)
sum(cases)/10^6

#4b
#use monte carlo to define prob of losing more than $1M
B <- 10000
set.seed(27, sample.kind = "Rounding")
cases_2 <- replicate(B, {
  claims <- sample( c(1150, -150000), n, prob=c(1-p_loss, p_loss), replace = TRUE) 
  sum(claims)<(-10^6)
})
mean(cases_2)

#~~~~~~~QUESTIONS 5 & 6: INSURANCE RATES, PART 3~~~~~~~~
#given the pandemic, find a premium which results in a prob of losing money at 5% given death rate is p = 0.015
p_lose_money = 0.05
n = 1000
p_loss = 0.015

#5a
#calculate premium required for 5% chance
l <- payout
z <- qnorm(p_lose_money)
x <- -l*( n*p_loss - z*sqrt(n*p_loss*(1-p_loss)))/ ( n*(1-p_loss) + z*sqrt(n*p_loss*(1-p_loss)))
x

#5b
#expected profit (value) per policy
payout*p_loss + x*(1-p_loss)    # expected value of the profit per policy


#5c
#expected profit (value) per 1k policies
n*(payout*p_loss + x*(1-p_loss))

#5d
set.seed(28, sample.kind = "Rounding")
#run a monte carlo to determine prob of losing money 10k times

B <- 10000
we_lose <- replicate(B, {
  claims <- sample( c(x, -150000), n, prob=c(1-p_loss, p_loss), replace = TRUE) 
  sum(claims)<0
})
mean(we_lose)

#6a
set.seed(29, sample.kind = "Rounding")
#company can't determine if death rate will remain stable. Use a sequence of death rates and Monte Carlo
profit <- replicate(B, {
  new_p <- p_loss + sample(seq(-0.01, 0.01, length = 100), 1)
  claims <- sample( c(x, payout), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(claims)
})
mean(profit)

#6b
#what is the probability of losing money
mean(profit < 0)    # probability of losing money

#6c
mean(profit < -10^6)    # probability of losing over $10 million
