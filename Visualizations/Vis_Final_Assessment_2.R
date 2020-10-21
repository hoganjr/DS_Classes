library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

#~~~~~QUESTION 1~~~~~~
str(temp_carbon)
temp_carbon %>% .$year %>% max()
temp_carbon %>% filter(!is.na(carbon_emissions))%>%pull(year)%>%max()
temp_carbon %>% filter(!is.na(carbon_emissions))%>%max(year)
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% .$year %>% max()
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% select(year) %>% max()
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% max(.$year)

#~~~~QUESTION 2~~~~~~~~
min_c<- temp_carbon %>% filter(!is.na(carbon_emissions))%>%pull(year)%>%min()
min_c
max_c<-temp_carbon %>% filter(!is.na(carbon_emissions))%>%pull(year)%>%max()
max_c
emiss <- temp_carbon %>% filter(!is.na(carbon_emissions)) 
last_year <- emiss$carbon_emissions[which.max(emiss$year)]
first_year <- emiss$carbon_emissions[which.min(emiss$year)]
last_year
first_year
last_year/first_year

#~~~~~~QUESTION 3~~~~~~~~
min_t<- temp_carbon %>% filter(!is.na(temp_anomaly))%>%pull(year)%>%min()
min_t
max_t<-temp_carbon %>% filter(!is.na(temp_anomaly))%>%pull(year)%>%max()
max_t
temp_anom <- temp_carbon %>% filter(!is.na(temp_anomaly)) 
last_t <- temp_anom$temp_anomaly[which.max(temp_anom$year)]
first_t <- temp_anom$temp_anomaly[which.min(temp_anom$year)]
last_t
first_t
last_t - first_t

#~~~~~QUESTION 4 & 5 & 6~~~~~~
p <- temp_carbon %>% filter(!is.na(temp_anomaly))%>% ggplot(aes(year,temp_anomaly))+geom_point()
#p + geom_vline(aes(xintercept = 0), col = "blue")
#p + geom_hline(aes(y = 0), col = "blue")
#p + geom_hline(aes(yintercept = 0, col = blue))
p + geom_hline(aes(yintercept = 0), col = "blue")+
  ylab("Temperature anomaly(degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y=0.05, label = "20th century mean"), col = "blue")


#~~~~~~QUESTION 7~~~~~~
#continue from above
p + geom_line(aes(year,ocean_anomaly), col = "red", label = "ocean") + geom_line(aes(year,land_anomaly), col = "green", label = "land") +
  geom_hline(aes(yintercept = 0), col = "blue")

#~~~~~QUESTION 8 & 9~~~~~~
greenhouse_gases %>%
  ggplot(aes(year,concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

#~~~~~~~QUESTION 10~~~~~~
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% ggplot(aes(year, carbon_emissions)) + geom_line()

#~~~~~~QUESTION 11~~~~~~~
co2_time <- historic_co2 %>% filter(!is.na(co2)) %>% ggplot(aes(year,co2, col = source)) + geom_line() +
  ggtitle("Atmospheric CO2 concentration, -800,000 BC to today") +
  ylab("co2 (ppmv)")
co2_time

#~~~~~QUESTION 12~~~~~~~~~
#continue from above
limit_x <- c(-3000, 2018)
co2_time <- historic_co2 %>% filter(!is.na(co2)) %>% ggplot(aes(year,co2, col = source)) + geom_line() +
  ggtitle("Atmospheric CO2 concentration, -800,000 BC to today") +
  ylab("co2 (ppmv)") + scale_x_continuous(limits = limit_x)
co2_time
