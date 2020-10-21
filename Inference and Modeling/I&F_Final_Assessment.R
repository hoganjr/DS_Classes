# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

#~~~~~~~BREXIT POLL ANALYSIS~~~~~~
p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

#Q1 - EXPECTED VALUE AND STANDARD ERROR OF A POLL

#consider a poll with a sample of N = 1500 voters
N <- 1500
#What is the expected total number of voters in the sample choosing "Remain"?
N*p
#What is the standard error of the total number of voters in the sample choosing "Remain"?
sqrt(N*p*(1-p))
#What is the expected value of  𝑋̂ , the proportion of "Remain" voters?
x_hat <- p
x_hat
#What is the standard error of  𝑋̂ , the proportion of "Remain" voters?
sqrt(x_hat*(1-x_hat)/N)
#What is the expected value of  𝑑 , the spread between the proportion of "Remain" voters and "Leave" voters?
d
#What is the standard error of  𝑑 , the spread between the proportion of "Remain" voters and "Leave" voters?
2*sqrt(p*(1-p)/N)

#Q2 - ACTUAL BREXIT POLL ESTIMATES

head(brexit_polls)
brexit_polls <- brexit_polls %>% mutate(x_hat = (spread + 1)/2 )
#What is the average of the observed spreads (spread)?
brexit_polls %>% summarize(avg = mean(spread))

#What is the standard deviation of the observed spreads?
brexit_polls %>% summarize(sd = sd(spread))

#What is the average of x_hat, the estimates of the parameter  𝑝 ?
brexit_polls %>% summarize(avg_x_hat = mean(x_hat))

#What is the standard deviation of x_hat?
brexit_polls %>% summarize(sd_x_hat = sd(x_hat))

#Q3 - CONFIDENCE INTERVAL OF A BREXIT POLL

brexit_polls[1,]$spread
#What is the lower bound of the 95% confidence interval?
se_x_hat <- sqrt(brexit_polls[1,]$x_hat*(1-brexit_polls[1,]$x_hat)/brexit_polls[1,]$samplesize)
brexit_polls[1,]$x_hat - qnorm(0.975)*se_x_hat

#What is the upper bound of the 95% confidence interval?
brexit_polls[1,]$x_hat + qnorm(0.975)*se_x_hat

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

#Q4 - CONFIDENCE INTERVALE FOR POLLS IN JUNE
june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01") %>% mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize)) %>% 
  mutate(se_spread = 2*se_x_hat, lower = spread - qnorm(0.975)*se_spread, upper = spread + qnorm(0.975)*se_spread) %>% mutate(hit = -0.038>=lower & -0.038<=upper)

#How many polls are in june_polls?
june_polls %>% summarize(n= n())

#What proportion of polls have a confidence interval that covers the value 0?
june_polls %>% mutate(CI_zero = (upper - lower)>=upper & upper > 0.0) %>% summarize(cross_zero = mean(CI_zero))

#What proportion of polls predict "Remain" (confidence interval entirely above 0)?
june_polls %>% mutate(CI_positive = lower > 0.0) %>% summarize(remain_predict = mean(CI_positive))

#What proportion of polls have a confidence interval covering the true value of  𝑑 ?
mean(june_polls$hit)

#Q5 - HIT RATE BY POLLSTER

june_polls %>% group_by(pollster) %>% summarize(n = n(), hits = mean(hit)) 

#Q6 - BOXPLOT OF BREXIT POLLS BY POLL TYPE

plot(june_polls$poll_type, june_polls$spread)

#Q7 - COMBINED SPREAD ACROSS POLL TYPE

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)
combined_by_type$spread[1]
#What is the lower bound of the 95% confidence interval for online voters?
combined_by_type$spread[1] - qnorm(0.975)*2*sqrt(combined_by_type$p_hat[1]*(1-combined_by_type$p_hat[1])/combined_by_type$N[1])
#What is the upper bound of the 95% confidence interval for online voters?
combined_by_type$spread[1] + qnorm(0.975)*2*sqrt(combined_by_type$p_hat[1]*(1-combined_by_type$p_hat[1])/combined_by_type$N[1])

#Q8 - INTERPRETING COMBINED SPREAD ESTIMATES ACROSS POLL TYPE

phone_ci_lower <- combined_by_type$spread[2] - qnorm(0.975)*2*sqrt(combined_by_type$p_hat[2]*(1-combined_by_type$p_hat[2])/combined_by_type$N[2])
phone_ci_lower
phone_ci_upper <- combined_by_type$spread[2] + qnorm(0.975)*2*sqrt(combined_by_type$p_hat[2]*(1-combined_by_type$p_hat[2])/combined_by_type$N[2])
phone_ci_upper

#Q9 - CHI-SQUARED P-VALUE

head(brexit_polls)

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit) %>% mutate(online_yes = ifelse(poll_type == "Online" & hit == TRUE,1,0), online_no = ifelse(poll_type == "Online" & hit == FALSE,1,0),
                                    phone_yes = ifelse(poll_type == "Telephone" & hit == TRUE,1,0), phone_no = ifelse(poll_type == "Telephone" & hit == FALSE,1,0))
  
brexit_hit

brexit_2x2 <- tibble(hit = c("no", "yes"),
                     online = c(sum(brexit_hit$online_no), sum(brexit_hit$online_yes)),
                     telephone = c(sum(brexit_hit$phone_no), sum(brexit_hit$phone_yes)))
                     
chisq_test <- brexit_2x2 %>%
  select(-hit) %>% chisq.test()
chisq_test$p.value

#Q10 - ODDS RATIO OF ONLINE AND TELEPHONE POLL HIT RATE

#Calculate the odds that an online poll generates a confidence interval that covers the actual value of the spread.
odds_online <- (brexit_2x2$online[2]/sum( brexit_2x2$online))/(brexit_2x2$online[1]/sum(brexit_2x2$online))
odds_online
#Calculate the odds that a telephone poll generates a confidence interval that covers the actual value of the spread.
odds_phone <- (brexit_2x2$telephone[2]/sum(brexit_2x2$telephone))/(brexit_2x2$telephone[1]/sum(brexit_2x2$telephone))
odds_phone
#Calculate the odds ratio to determine how many times larger the odds are for online polls to hit versus telephone polls.
odds_online/odds_phone

#Q11 - PLOTTING SPREAD OVER TIME

head(brexit_polls)
brexit_polls %>%
  select(enddate, poll_type, spread) %>%
  group_by(poll_type) %>%
  ggplot(aes(enddate, spread, color = poll_type)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_line(y=-0.038)

#Q12 - PLOTTING RAW PERCENTAGES OVER TIME

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))%>% select(enddate, proportion, vote) %>% ggplot(aes(enddate, proportion, color = vote)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.3) 
brexit_long