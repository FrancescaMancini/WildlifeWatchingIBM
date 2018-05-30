# IBM scenario 1: Code of Conduct ####
# Author: Francesca Mancini
# Date created: 2018-05-30
# Date modified: 

library(dplyr)
source("IBMfunctions.R")

# initialise population of tourists and tour operators ####
set.seed(123)

# years months and days
years <- 1
months <- 12
days <- 365

# fine
#fine <- 1000

# create vector of behavioural phenotypes for tour operators

phenotypes <- c("trustful", "optimist", "pessimist", "envious", "undefined")

# the phenotypes will be sampled from a population with the following prportions of phenotypes
# Poncela-Casasnovas et al, 2016. Science Advances 2(8): e1600451-e1600451. doi:10.1126/sciadv.1600451

p_trustful <- 0.17
p_optimist <- 0.2
p_pessimist <- 0.21
p_envious <- 0.3
p_undefined <- 0.12


tour_ops <- data.frame(id = seq(1, 10, 1), price = runif(10, 15, 25), rating = runif(10, 3, 5),
                      capacity = as.integer(runif(10, 10, 30)), bookings = rep(0, 10), 
                      investment_infra = rep(0, 10), investment_ot = rep(0, 10), 
                      time_with = rep(0, 10), profit = rep(0, 10), profit_year = rep(0, 10),
                      phenorype = sample(phenotypes, 10, replace=TRUE, 
                                         prob=c(p_trustful, p_optimist, p_pessimist, p_envious, p_undefined)),
                      behaviour = character(10), extra_costs = rep(0, 10), 
                      tours = rep(365, 10), stringsAsFactors = FALSE)

tourists_pop <- data.frame(id = seq(1, 10000, 1), price_max = runif(10000, 12, 25),
                       rating_min = runif(10000, 2, 3.5), going = rep(NA, 10000), 
                       waiting = rep(0, 10000), satisfaction = rep(NA, 10000), 
                       satis_animals = rep(NA, 10000), satis_price = rep(NA, 10000), 
                       satis_infr = rep(NA, 10000), satis_invest = rep(NA, 10000),
                       satis_wait = rep(NA, 10000), stringsAsFactors=FALSE)

#tourists_months <- c(50, 50, 200, 800, 1230, 1830, 2210, 2700, 1140, 773, 97, 50)

# create dataframe to store history of behavioural strategies

behaviours <- data.frame(id = seq(1, 10, 1), cooperation = rep(0, 10), defection = rep(0, 10))

# create vectors to store wildlife and management parameters

effects <- rep(NA, years)

encounter_probs <- rep(NA, years)
encounter_probs[1] <- 0.6

# time_allowed <- rep(NA, years)
# 
# tourists_allowed <- rep(NA, years)

max_times <- rep(NA, years)
max_times[1] <- 10000

slope_effect <- 15

# calculates encounter probabilities 
# and maximum time that can be sustainably be spent with animals
# according to effect of tourism in the previous year

max_times <- ifelse(y == 1, max_times[y], time_with_animals(maxx = max_times[y-1], effect = effects[y-1])) 

encounter_probs <- ifelse(y == 1, encounter_probs[y], p_encounter(p_e = encounter_probs[y-1], effect = effects[y-1]))

# tour operators update prices according to costs

tour_ops <- tour_ops %>%
  mutate(price = ifelse(y == 1, price, price_change(extra_cost = extra_costs, ntours = tours)))

# for(m in 1:months){
#   tourists <- sample(tourists, 10, replace=FALSE, 
#                                          prob=c(p_trustful, p_optimist, p_pessimist, p_envious, p_undefined))
# }

# need to find a way to simulate daily time series with seasonality that reflects real seasonality 
# and trends that simulate increasing, decreasing or fluctuating demand