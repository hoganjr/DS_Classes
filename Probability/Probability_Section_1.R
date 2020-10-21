#~~~~~MONTE CARLO SIMULATIONS~~~~~~~
beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions

#~~~~~~COMBINATIONS AND PERMUTATIONS~~~~~
#introducing paste() and expand.grid()
# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

#generating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

#permutations and combinations
install.packages("gtools")
library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

#probability of drawing a second king given that one king is drawn
hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

#probability of a natural 21 in blackjack (Ace & face)
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

#Monte Carlo simulation of natural 21 in blackjack
# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

#~~~~~THE BIRTHDAY PROBLEM~~~~~~~~~~
# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays

#~~~~~~~SAPPLY~~~~~~~~~~
#functions for birthday problem Monte Carlo simulations
# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

#Element-wise operation over vectors and sapply
n <- seq(1, 60)
x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)

#Computing birthday problem probabilities wit sapply
# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob

#~~~~~~HOW MANY MONTE CARLO EXPERIMENTS IS ENOUGH?~~~~~~~
B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 

#~~~~~~THE MONTE HALL PROBLEM~~~~~~~~~~
#Monte Carlo simulation of sticking to the same door
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking

#Monte Carlo simulation of switching doors
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching


#~~~~~ASSESSMENT~~~~~~~
library(gtools)
libraty(tidyverse)

#Q1
#how many ways to distribute 3 medals across 8 runners
order <- permutations(8, 3, repeats.allowed = FALSE)
nrow(order)

#how many ways can three medals go to the 3 Jamaicans
order_jamaicans <- permutations(3, 3, repeats.allowed = FALSE)
nrow(order_jamaicans)

#what is probability that all 3 jamaicans medal?
3/8*2/7*1/6

#use Monte Carlo to calculate this probability
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000
set.seed(1)
results <- replicate(B, {
  finish <- sample(runners) #randomly reorder runners
  finish[1] == "Jamaica" & finish[2] == "Jamaica" & finish[3] == "Jamaica"
})
mean(results)

#Q2
#meal combos possible with current menu
entrees <- c("e1","e2","e3","e4","e5","e6")
sides <- c("s1","s2","s3","s4","s5","s6")
drinks <- c("d1","d2")
meal <- expand.grid(entree = entrees, side1 = sides, side2 = sides, drink = drinks)
6*2*nrow(combinations(6,2))

#manager adds 1 drink
6*3*nrow(combinations(6,2))

#manager also allows 3 sides
6*3*nrow(combinations(6,3))

#manager changes back to 2 sides and wants to add entree, whats the min # entrees to get 365 combinations
combos <- function(N) {
  options <- N*3*nrow(combinations(6,2))
  options >= 365
}
sapply(1:12,combos)

#what about expanding # of sides instead of entrees, how many sides required?
combos <- function(N) {
  options <- 6*3*nrow(combinations(N,2))
  options >= 365
}
sapply(2:12,combos)

#Q3
library(tidyverse)
head(esoph)
str(esoph) 

all_cases <- esoph %>% summarize(sum(ncases))

all_controls <- esoph %>% summarize(sum(ncontrols))

#Q4
#prob that highest alcohol consumption group member is cancer case
highest_alcgp_case <- esoph %>% filter(alcgp == "120+") %>% summarize(sum(ncases))
highest_alcgp_control <- esoph %>% filter(alcgp == "120+") %>% summarize(sum(ncontrols))
highest_alcgp_case/(highest_alcgp_case + highest_alcgp_control)

#prob that lowest alcohol consumption group member is cancer case
low_alcgp_case <- esoph %>% filter(alcgp == "0-39g/day") %>% summarize(sum(ncases))
low_alcgp_control <- esoph %>% filter(alcgp == "0-39g/day") %>% summarize(sum(ncontrols))
low_alcgp_case/(low_alcgp_case + low_alcgp_control)

#given that a person is a case, what's the prob they smoke 10g+ a day?
smokes_more <- esoph %>% filter(ncases > 0 & tobgp != "0-9g/day") %>% summarize(sum(ncases))
smokes_more/all_cases

#given that a person is a control, what's the prob they smoke 10g+ a day?
cntrl_smokes_more <- esoph %>% filter(ncontrols > 0 & tobgp != "0-9g/day") %>% summarize(sum(ncontrols))
cntrl_smokes_more/all_controls

#Q5
#for cases what's the prob of being in the highes alcohol group
highest_alcgp_case/all_cases

#for cases, what's the prob of being in the highest tobacco group
levels(esoph$tobgp)
highest_tobgp_case <- esoph %>% filter(tobgp == "30+") %>% summarize(sum(ncases))
highest_tobgp_case/all_cases

#for cases, what's prob of being in highest tobacco and alcohol group
highest_case_combined <- esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>% summarize(sum(ncases))
highest_case_combined/all_cases

#for cases, prob of highest alc group or highest tob group
highest_case_either <- esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>% summarize(sum(ncases))
case_either <- highest_case_either/all_cases

#Q6
#prob of controls in highest group
highest_alcgp_control/all_controls
#times more likely are cases than cntrls in highest alc grp
(highest_alcgp_case/all_cases)/(highest_alcgp_control/all_controls)
#prob of cntrls in highest tobacco grp
highest_tobgp_cntrl <- esoph %>% filter(tobgp == "30+") %>% summarize(sum(ncontrols))
highest_tobgp_cntrl/all_controls
#for cntrls, prob of highest alc grp and tob grp
highest_cntrl_combined <- esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>% summarize(sum(ncontrols))
highest_cntrl_combined/all_controls
#for cntrls, prob of highest alc grp OR tob grp
highest_cntrl_either <- esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>% summarize(sum(ncontrols))
cntrl_either <- highest_cntrl_either/all_controls
#how many times more like are cases than cntrls to be in highest alc grp or tob grp?
case_either/cntrl_either
