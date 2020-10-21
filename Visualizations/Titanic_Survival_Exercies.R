install.packages("titanic")
library(dslabs)
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

#~~~~QUESTION 2~~~~~~~
titanic %>% ggplot(aes(Age, y = ..count.., fill = Sex)) + geom_density(alpha = 0.2, bw = 0.75)

#~~~~QUESTION 3~~~~~~
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(sample = Age))+geom_qq(dparams = params)+geom_abline()

#~~~~~QUESTION 4~~~~~~~
titanic %>% filter(!is.na(Survived) & !is.na(Sex)) %>% ggplot(aes(Survived, fill = Sex))+
  geom_bar(position = position_dodge())

#~~~~QUESTION 5~~~~~~~~
titanic %>% ggplot(aes(Age, y = ..count.., fill = Survived)) + geom_density(alpha = 0.2, bw = 0.75)

#~~~~~~QUESTION 6~~~~~~
titanic %>% filter(Fare >= 0 & !is.na(Fare) & !is.na(Sex)) %>% ggplot(aes(Survived, Fare))+geom_boxplot()+
  scale_y_continuous(trans = "log2") +
  geom_jitter(width = 0.1, alpha = 0.2)

#~~~~~QUESTION 7~~~~~~
titanic %>% filter(!is.na(Survived) & !is.na(Pclass)) %>% ggplot(aes(Pclass, fill = Survived))+
  geom_bar()

titanic %>% filter(!is.na(Survived) & !is.na(Pclass)) %>% ggplot(aes(Pclass, fill = Survived))+
  geom_bar(position = position_fill())

titanic %>% filter(!is.na(Survived) & !is.na(Pclass)) %>% ggplot(aes(Survived, fill = Pclass))+
  geom_bar(position = position_fill())

#~~~~~QUESTION 8~~~~~~~~
titanic %>% filter(!is.na(Age) & !is.na(Sex) & !is.na(Survived) & !is.na(Pclass)
                   ) %>% ggplot(aes(Age, y = ..count.., fill = Survived)) + geom_density(alpha = 0.2, bw = 0.75) +
  facet_grid(Sex~Pclass)
