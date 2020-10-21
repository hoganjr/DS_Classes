library(tidyverse)
library(dslabs)
library(dplyr)
data(murders)

#ggplot(data=murders) <---automatically generates a plot since no object is assigned
#murders %>% ggplot() <-- automatically generates a plot since no object is assigned

p <- ggplot(data = murders)
class(p)

#~~~layers~~~~~
murders %>% ggplot() + geom_point(aes(x=population/10^6, y = total))

#add points layer to predefined ggplot object
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6,total))

#add text to layer to scatterplot
p + geom_point(aes(population/10^6,total)) + geom_text(aes(population/10^6, total, label = abb))


#~~~~Tinkering~~~~~
#change the size of the points
p + geom_point(aes(population/10^6, total), size = 3)+
  geom_text(aes(population/10^6, total, label = abb))

#move text labels slightly to the right
p + geom_point(aes(population/10^6, total), size = 3)+
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)

#simplify code by adding global aesthetic
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) + geom_text(nudge_x = 1.5)

#~~~Scales, Labels, and Colors~~~~
#log base 10 scale the x and y axis
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) + #adjust the nudge for log scale
  scale_x_continuous(trans = "log10") +
  scale_y_condinuous(trans = "log10")
#ggplot has log10 built in for scaling
p <- murders %>%ggplot(aes(population/10^6, total, label = abb))+
  geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10()

#add labels and titls
p + xlab("Population in millions (log scale)") +
  ylab("Total number of muders (log scale)")+
  ggtitle("US Gun Murders in 2010")

#change color of the points
p + geom_point(size = 3, color = "blue") #makes all points blue
p + geom_point(aes(col = region), size = 3) #colors points by region

#add a line w/ average murder rte
r <- murders %>% summarize(rate = sum(total)/sum(population) * 10^6) %>% pull(rate)

#basic line with average murder rate for the country
p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r)) #slope is default of 1

#change line to dashed and dark grey, line under data points
p + geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3)

#change legend title
p+scale_color_discrete(name = "Region") #capitalize legend title
p

#~~~~Add on packages~~~~~
install.packages(c("ggrepel","ggthemes"))
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
library(dplyr)
ds_theme_set()
data(murders)
#define the intercept
r<- murders %>% summarize(rate = sum(total)/ sum(population)*10^6) %>% .$rate
#make the plot, combining all elements
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() + #changes point labeling 
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") + #changes name of legend
  theme_economist() #loads "Economist" theme from ggthemes (also _fivethirtyeight)

#~~~~~Other Examples~~~~~
#HISTOGRAMS
# load heights data
library(tidyverse)
library(dslabs)
data(heights)

# define p
p <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height))

# basic histograms
p + geom_histogram()
p + geom_histogram(binwidth = 1)

# histogram with blue fill, black outline, labels and title
p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Male heights in inches") +
  ggtitle("Histogram")

#SMOOTH DENSITY PLOT
p + geom_density()
p + geom_density(fill = "blue")

#QUANTILE-QUANTILE(QQ) PLOT
# basic QQ-plot
p <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))
p + geom_qq()

# QQ-plot against a normal distribution with same mean/sd as data
params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
  geom_abline()

# QQ-plot of scaled data against the standard normal distribution
heights %>% ggplot(aes(sample = scale(height))) +
           geom_qq() +
           geom_abline()

#GRID OF PLOTS with gridExtra package
# define plots p1, p2, p3
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")

# arrange plots next to each other in 1 row, 3 columns
install.packages("gridExtra")
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)