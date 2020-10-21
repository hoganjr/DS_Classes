library(dslabs)
library(tidyverse) 
library(Lahman)

#~~~~~~CONFOUNDING: ARE BB'S MORE PREDICTIVE?~~~~~

# find regression line for predicting runs from BBs
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))

#~~~~~~~STRATIFICATION AND MULTIVARIATE REGRESSION~~~~~~~~

# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game))

#~~~~~~~~~~LEAST SQUARES ESTIMATES (LSE)~~~~~~~~~~~

# compute RSS for any pair of beta0 and beta1 in Galton's data
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# plot RSS as a function of beta1 when beta0=25
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

#~~~~~~~~THE LM() FUNCTION~~~~~~~~

# fit regression line to predict son's height from father's height
fit <- lm(son ~ father, data = galton_heights)
fit

# summary statistics
summary(fit)

#~~~~~~~~~LSE ARE RANDOM VARIABLES~~~~~~

# Monte Carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

# Plot the distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

# summary statistics
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>%
  .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

#~~~~~~~~ADVANCED NOTE ON LSE~~~~~~

#Although interpretation is not straight-forward, it is also useful to know 
#that the LSE can be strongly correlated, which can be seen using this code:

lse %>% summarize(cor(beta_0, beta_1))

#However, the correlation depends on how the predictors are defined or transformed.
#Here we standardize the father heights, which changes x_i to x_i-x_bar.

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})
cor(lse[1,], lse[2,])

#~~~~~~~PREDICTED VALUES ARE RANDOM VARIABLES~~~~~~~~~

# plot predictions and confidence intervals
galton_heights %>% ggplot(aes(son, father)) +
  geom_point() +
  geom_smooth(method = "lm") #<--- generates shaded area for confidence intervals

# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()

#~~~~~~~SECTION 2.2 ASSESSMENT~~~~~~~~

#Q1
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

#Q3
library(Lahman)
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(R_game = R/G, BB_game = BB /G, HR_game = HR / G) %>%  
  lm(R_game ~ (BB_game + HR_game), data = .) %>% .$coeff

#Q6
#plot option 2
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

#plot option 3
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

#Q7-8
set.seed(1989, sample.kind="Rounding")
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight) 

fit_w <- lm(mother ~ daughter, data = female_heights)
fit_w

mother_predict <- predict(fit_w, se.fit = TRUE)
mother_predict$fit[1]
female_heights$mother[1]


#Q9
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)


bat_99 <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>% 
  filter(pa >= 100) %>% select(playerID, singles, bb) %>% group_by(playerID) %>% 
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))
bat_99  
mean(bat_99$mean_singles > 0.2)*nrow(bat_99)
mean(bat_99$mean_bb > 0.2)*nrow(bat_99)

#Q10
bat_9902 <- inner_join(bat_02, bat_99)

cor(bat_9902$singles, bat_9902$mean_singles)
cor(bat_9902$bb, bat_9902$mean_bb)

#Q11

at_9902 %>% ggplot(aes(bat_9902$singles, bat_9902$mean_singles)) + geom_point()
bat_9902 %>% ggplot(aes(bat_9902$bb, bat_9902$mean_bb)) + geom_point()

#Q12
fit_singles <- lm(bat_9902$singles ~ bat_9902$mean_singles, data = bat_9902)
fit_singles
fit_bb <- lm(bat_9902$bb ~ bat_9902$mean_bb, data = bat_9902)
fit_bb

#~~~~~~ADVANCED DPLYR: TIBBLES~~~~~~~~~

# stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

# calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

# use lm to get estimated slopes - lm does not work with grouped tibbles
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

# inspect a grouped tibble
dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()

#~~~~~~TIBBLES: DIFFERENCES FROM DATA FRAMES~~~~~~~~~

# inspect data frame and tibble
library(Lahman)
Teams
as_tibble(Teams)

# subsetting a data frame sometimes generates vectors
class(Teams[,20])

# subsetting a tibble always generates tibbles
class(as_tibble(Teams[,20]))

# pulling a vector out of a tibble
class(as_tibble(Teams)$HR)

# access a non-existing column in a data frame or a tibble
Teams$hr
as_tibble(Teams)$hr

# create a tibble with complex objects
tibble(id = c(1, 2, 3), func = c(mean, median, sd))

#~~~~~~~~DO() FUNCTION~~~~~~~~~

# use do to fit a regression line to each HR stratum
dat %>%  
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))

# using do without a column name gives an error
dat %>%
  group_by(HR) %>%
  do(lm(R ~ BB, data = .))

# define a function to extract slope from lm
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}

# return the desired data frame
dat %>%  
  group_by(HR) %>%
  do(get_slope(.))

# not the desired output: a column containing data frames
dat %>%  
  group_by(HR) %>%
  do(slope = get_slope(.))

# data frames with multiple rows will be concatenated appropriately
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficients, 
             se = summary(fit)$coefficient[,2])
}

dat %>%  
  group_by(HR) %>%
  do(get_lse(.))

#~~~~~~~~BROOM~~~~~~~~~~

# use tidy to return lm estimates and related information as a data frame
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)

# add confidence intervals with tidy
tidy(fit, conf.int = TRUE)

# pipeline with lm, do, tidy
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

# make ggplots
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

# inspect with glance
glance(fit)

#~~~~~~~~SECTION 2.3 ASSESSMENT~~~~~~~~~~~

#library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

#Q8

dat8 <- galton %>% group_by(pair) %>% summarize(n())
dat8

#Q9
dat9 <- galton %>% group_by(pair) %>% summarize(number = n(), cor_coeff = cor(childHeight, parentHeight))
dat9

#Q10
dat10 <- galton %>% group_by(pair) %>%  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight") #%>%
  #select(HR, estimate, conf.low, conf.high)
dat10
dat10 %>% select(pair, estimate, conf.low, conf.high) %>%
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

#~~~~~~~~BUILDING A BETTER OFFENSIVE METRIC FOR BASEBALL~~~~~~~~

# linear regression with two variables
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

# predict number of runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

# average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

#A way to actually pick the players for the team can be 
#done using what computer scientists call linear programming. 
#Although we don't go into this topic in detail in this course, 
#we include the code anyway:

library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE)

#This algorithm chooses these 9 players:
our_team <- players %>% filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

#We note that these players all have above average BB and HR rates while the same is not true for singles.
my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))

#~~~~~~~REGRESSION FALLACY~~~~~~~~

#The code to create a table with player ID, their names, and their most played position:
library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

#The code to create a table with only the ROY award winners and add their batting statistics:
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

#The code to keep only the rookie and sophomore seasons and remove players who did not play sophomore seasons:
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

#The code to use the spread function to have one column for the rookie and sophomore years batting averages:
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY

#The code to do the similar analysis on all players that played the 2013 and 2014 seasons and batted 
#more than 130 times (minimum to win Rookie of the Year):
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years

#The code to see what happens to the worst performers of 2013:
arrange(two_years, `2013`)

#The code to see  the correlation for performance in two separate years:
qplot(`2013`, `2014`, data = two_years)

summarize(two_years, cor(`2013`,`2014`))

#~~~~~~~~~~MEASUREMENT ERROR MODELS~~~~~~~~~~

#The code to use dslabs function rfalling_object to generate simulations of dropping balls:
library(dslabs)
falling_object <- rfalling_object()

#The code to draw the trajectory of the ball:
falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")

#The code to use the lm() function to estimate the coefficients:
library(broom)
fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)

tidy(fit)

#The code to check if the estimated parabola fits the data:
augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")

#The code to see the summary statistic of the regression:
tidy(fit, conf.int = TRUE)

#~~~~~~~SECTION 2.4 ASSESSMENT~~~~~~~~~~

#Q9
library(Lahman)
library(broom)
fit_q9 <- Teams %>% 
  filter(yearID %in% 1971) %>% 
  mutate(BB = BB / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB  + HR, data = .)
coefs_q9 <- tidy(fit_q9, conf.int = TRUE)
coefs_q9

#Q10
Library(Lahman)
dat_q10 <- Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  mutate(BB = BB / G, 
         HR = HR / G,
         R = R / G)

fit_q10 <- lm(R ~ BB, data = dat_q10)
tidy(fit_q10)

dat_q10 %>%  
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(yearID, estimate, conf.low, conf.high) %>%
  ggplot(aes(yearID, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point() + geom_smooth()

#Q11
dat_q11 <- dat_q10 %>%  
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(yearID, estimate, conf.low, conf.high)
fit_q11 <- lm(estimate ~ yearID, data = dat_q11)
tidy(fit_q11)

fit_q11 <- lm(yearID ~ estimate, data = dat_q11)
tidy(fit_q11)

#~~~~~~~SECTION 2 OVERALL ASSESSMENT~~~~~~~~

library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

#Q1a
Teams_small_q1 <- Teams_small %>% 
  mutate(R_per_game = R / G, HR_per_game = HR / G) 

Teams_small_q1%>%
  lm(avg_attendance ~ R_per_game, data = .) %>% 
  .$coef %>%
  .[2]

Teams_small_q1%>%
  lm(avg_attendance ~ HR_per_game, data = .) %>% 
  .$coef %>%
  .[2]

#Q1b
Teams_small %>% lm(avg_attendance ~ W, data = .) %>% .$coeff

#Q1c
Teams_small %>% lm(avg_attendance ~ yearID, data = .) %>% .$coeff %>% .[2]

#Q2
Teams_small_q1 %>% summarize(cor(W, R_per_game), cor(W, HR_per_game))

#Q3a
Teams_small_q3 <- Teams_small_q1 %>% 
  mutate(W_strata = round(W/10, 0)) %>%
  filter(W_strata >= 5 & W_strata <=10)
Teams_small_q3 %>% filter(W_strata == 8) %>% summarize(n = n())

#Q3b
Teams_small_q3 %>%
  group_by(W_strata) %>%
  summarize(slope = cor(R_per_game, avg_attendance)*sd(avg_attendance)/sd(R_per_game))

Teams_small_q3 %>%
  group_by(W_strata) %>%
  summarize(slope = cor(HR_per_game, avg_attendance)*sd(avg_attendance)/sd(HR_per_game))  

#Q3c
Teams_small_q3 %>%
  group_by(W_strata) %>%
  summarize(cor(R_per_game, avg_attendance), cor(HR_per_game, avg_attendance))

#Q4
Teams_small_q4 <- Teams_small_q1 %>% lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = .)
tidy(Teams_small_q4) %>% filter(term == "R_per_game") %>% pull(estimate)

#Q5
predict(Teams_small_q4, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 2002))

predict(Teams_small_q4, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 1960))

#Q6
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(avg_attendance = attendance / G, R_per_game = R / G, HR_per_game = HR / G)  %>% 
  mutate(avg_hat = predict(Teams_small_q4, newdata = .)) %>% summarize(cor(avg_hat, avg_attendance))
