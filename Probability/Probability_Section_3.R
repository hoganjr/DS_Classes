library(tidyverse)
library(dslabs)
install.packages("gtools")
library(gtools)

#~~~~RANDOM VARIABLES~~~~~~~~~~~~

# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)

#~~~~~~SAMPLING MODELS~~~~~~~
# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

# use the sampling model to run a Monte Carlo sim and use the results to estimate probability of losing money
n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money

#plot the histogram of the observed S values as well as the normal densitry curve based on mean and std dev.
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")


#~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~ASSESSMENT~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~

#Q1
points_right = 1
points_wrong = -0.25
p_right = 1/5
p_wrong = 1-p_right
#a
#probability for guessing correctly on one question
1/5
#b
#expected value of guessing on one question (right = +1, wrong = -0.25)
points_right*p_right + points_wrong*p_wrong
#c
#expected score of guessing on all 44 SAT questions
44*(points_right*p_right + points_wrong*p_wrong)
#d
#std err of guessing on all 44
(abs(points_wrong-points_right)*sqrt(p_right * p_wrong))*sqrt(44)
#e
#using CLT determine the probability that a guessing student scores 8 or higher
1-pnorm(8, 44*(points_right*p_right + points_wrong*p_wrong), (abs(points_wrong-points_right)*sqrt(p_right * p_wrong))*sqrt(44))
#f
#create a Monte Carlo of 10k students taking the test
set.seed(21)
B <- 10000
S <- replicate(B, {
  X <- sample(c(-0.25,1), 44, replace = TRUE, prob = c(p_wrong, p_right))    
  sum(X)    # determine total points
})
mean(S >= 8)    

#Q2
#options changed from 5 to 4 and incorrect penalty is dropped
points_right2 = 1
points_wrong2 = 0
p_right2 = 1/4
p_wrong2 = 1-p_right2
#a
#expected value of the score when guessing on the new test
44*(points_right2*p_right2 + points_wrong2*p_wrong2)
#b
#range of correct answer probabilities
p <- seq(0.25, 0.95, 0.05)
scoring <- function(N) {
  X <- sample(c(points_wrong2, points_right2), 44, replace = TRUE, prob = c(1-N, N))
  sum(X)>35
}
sapply(p, scoring)

#Q3
#A casino offers a House Special bet on roulette, which is a bet on five pockets 
#(00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 to 1. In other words, 
#a losing bet yields -$1 and a successful bet yields $6. A gambler wants to know the 
#chance of losing money if he places 500 bets on the roulette House Special.
lose_bet = -1
win_bet = 6
p_win = 5/38
p_lose = 1-p_win
#a
#expected value of the payout for one bet
win_bet*p_win + lose_bet*p_lose
#b
#standard error of the payout for one bet
abs(lose_bet - win_bet)*sqrt(p_win*p_lose)
#c
#expected value of the average of 500 bets
win_bet*p_win + lose_bet*p_lose
#d
#std error of the average of the payout over 500 bets
(abs(lose_bet - win_bet)*sqrt(p_win*p_lose))/sqrt(500)
#e
#expected value of the sum of 500 bets
500*(win_bet*p_win + lose_bet*p_lose)
#f
#std error of the sum of 500 bets
sqrt(500)*(abs(lose_bet - win_bet)*sqrt(p_win*p_lose))
#g
#use pnorm() withe expected value of the sum and std error of the sum to calculate
#the probability of losing money over 500 bets Pr(X<=0)
pnorm(0, 500*(win_bet*p_win + lose_bet*p_lose), sqrt(500)*(abs(lose_bet - win_bet)*sqrt(p_win*p_lose)))
