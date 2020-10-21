#~~~~~~~CONTINUOUS PROBABILITY~~~~~
#define x as the male heights from the dslabs heights dataset
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
#Given a vector x, we can define a function for computing the CDF of x using:
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches

#~~~~~~THEORETICAL DISTRIBUTION~~~~~~
#using pnorm() to calculate probabilities
#Given male heights x:
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
#we can estimate the probability that a male is taller than 70.5 inches using:
1 - pnorm(70.5, mean(x), sd(x))

#Discretization and the normal approximation
# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

#~~~~~~~PROBABILITY DENSITY~~~~~~
library(tidyverse)
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()
#note that dnorm() gives the densities for the std normal distrib by default. Probabilites for
#alternative normal distribs w/ mean mu and std dev sigma can be evaluated with
#dnorm(z, mu, sigma)

#~~~~~MONTE CARLO SIMULATION~~~~~
#generating normally distributed random numbers
# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

#Monte Carlo simulation of tallest person over 7ft
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)

#~~~~~OTHER CONTINUOUS DISTRIBUTIONS~~~~~~
#Plotting the normal distribution with dnorm
x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()


#~~~~~~~~~ASSESSMENT~~~~~~~~~~~~
set.seed(16)
act_avg = 20.9
act_sd = 5.7
act_scores <- rnorm(10000, act_avg, act_sd)

#1a
mean(act_scores)
#1b
sd(act_scores)
#1c
#how many perfect scores (36)
sum(act_scores >= 36)
#1d
#prob of score greater than 30
mean(act_scores > 30)
#1e
#prob of score less than or equal to 10
mean(act_scores <= 10)

#2
x <- 1:36
data.frame(x, f_x = dnorm(x, 20.9, 5.7)) %>%
  ggplot(aes(x,f_x)) +
  geom_line()

#3a
Z_scores <- (act_scores - mean(act_scores))/sd(act_scores)
mean(Z_scores > 2)
#3b
#what score is represents Z=2
2*sd(act_scores)+mean(act_scores)
#3c
qnorm(0.975, mean(act_scores), sd(act_scores))

#4a
N <- 1:36
score_prob <- function(N){
  mean(act_scores <= N)
}
1-sapply(N,score_prob)
#4b
qnorm(0.95, 20.9, 5.7)
#4c
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores,p)
sample_quantiles
#4d
theoretical_quantiles <- qnorm(p,20.9,5.7)
ggplot(aes(theoretical_quantiles, sample_quantiles))+geom_qq()
