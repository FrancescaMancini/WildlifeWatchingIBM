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
# months <- 12
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
                      investment_infra = rep(0.001, 10), investment_ot = rep(0.001, 10), 
                      time_with = rep(0, 10), time_with_year = rep(0, 10), 
                      profit = rep(0, 10), profit_year = rep(0, 10),
                      phenotype = sample(phenotypes, 10, replace=TRUE, 
                                         prob=c(p_trustful, p_optimist, p_pessimist, p_envious, p_undefined)),
                      behaviour = character(10), tours = rep(365, 10), stringsAsFactors = FALSE)



# generate daily time series of tourists
tourists_start_mean <- 100
days_tot <- years* days  # number of days in total

#effect sizes
y_effect <- 0.005          # trends in demand
eff.season <- 50        # seasonal fluctuation
season.sd <- 15
# sampling noise
sampling.sd <-10

# creates a vector holding the day of the year for all of the days
season <- rep(1:days, length = days_tot)
# creates a vector holding the days
days_all <- 1:days_tot

# calculate the number of tourists as a linear function of the annual trend
# and a sinusoidal function of the day of year
# plus some noise

n_tourists <- round(rnorm(days_tot, 
                          mean = tourists_start_mean + y_effect *days_all - rnorm(days_tot, eff.season, season.sd) * cos(2 * pi/days*season), 
                          sd =sampling.sd))


# create dataframe to store history of behavioural strategies

#behaviours <- data.frame(id = seq(1, 10, 1), cooperation = rep(0, 10), defection = rep(0, 10))

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


#for(y in 1:years){                                  # start year loop

# create year tourists population
tourists_pop <- data.frame(id = seq(1, 1000000, 1), price_max = runif(1000000, 12, 25),
                       rating_min = runif(1000000, 2, 3.5), going = rep(NA, 1000000), 
                       waiting = rep(0, 1000000), sample_p = rep(0.5, 1000000), 
                       satisfaction = rep(NA, 1000000), 
                       satis_animals = rep(NA, 1000000), satis_price = rep(NA, 1000000), 
                       satis_infr = rep(NA, 1000000), satis_invest = rep(NA, 1000000),
                       satis_wait = rep(NA, 1000000), stringsAsFactors=FALSE)

# calculates encounter probabilities 
# and maximum time that can be sustainably be spent with animals
# according to effect of tourism in the previous year

max_times <- ifelse(y == 1, max_times[y], time_with_animals(maxx = max_times[y-1], effect = effects[y-1])) 

encounter_probs <- ifelse(y == 1, encounter_probs[y], p_encounter(p_e = encounter_probs[y-1], effect = effects[y-1]))

# tour operators update prices according to costs

tour_ops <- tour_ops %>%
  mutate(price = case_when(y > 1 ~ price_change(extra_cost = investment_infra + investment_ot, ntours = tours), 
                           TRUE ~ price))

#for(d in 1:days){

day_of_sim <- d + (y - 1) * 365

### daily tourists here

tourists <- tourists_pop[sample(nrow(tourists_pop), n_tourists[day_of_sim], replace = FALSE, prob = tourists_pop$sample_p), ]



bookings <- booking(tourists, as.data.frame(tour_ops))

# extract dataframes from list
tourists <- bookings[[1]]
tour_ops <- bookings[[2]]

# avoid tour operators' profits being negative
# if booking * price - costs < 0 then tour operator does not run tour
tour_ops$bookings <- ifelse(((tour_ops$bookings * tour_ops$price) - (0.7 * 90)) < 0, 0, tour_ops$bookings)

# calculate time spent with animals
tour_ops$time_with <- ifelse(tour_ops$bookings == 0, 0, 
                             encounter_time(n_ops = length(which(tour_ops$bookings != 0)), p_e = encounter_probs[1]))
  
  
# tour operators calculate time spent with animals and profits
tour_ops <- tour_ops %>%
  mutate(time_with_year = time_with_year + time_with,
    profit = case_when(bookings == 0 ~ 0, 
                       TRUE ~ (bookings * price) - (0.7 * 90)),
    profit_year = profit_year + profit)

# tourists calculate satisfaction
tourists <- tourists %>%
  group_by(going) %>%
  mutate(satis_animals = ifelse(is.na(going), as.integer(NA), 
                                satisfaction_animals(tour_ops[which(tour_ops$id == unique(going)), "time_with"], 90, 15)),
         satis_price = ifelse(is.na(going), as.integer(NA), 
                              satisfaction_price(tour_ops[which(tour_ops$id == unique(going)), "price"], max(tour_ops$price),
                                                 tour_ops[which(tour_ops$id == unique(going)), "rating"], max(tour_ops$rating), 15, 0.7)),
         satis_wait = ifelse(is.na(going), as.integer(NA), satisfaction_waiting(waiting, -60,0.1)),
         satis_infr = ifelse(is.na(going), as.integer(NA), 
                             satisfaction_infr_investment(tour_ops[which(tour_ops$id == unique(going)), "investment_infra"], 
                                                          max(tour_ops$investment_infra), 10, 0.3)),
         satis_invest = ifelse(is.na(going), as.integer(NA), 
                              satisfaction_other_investment(tour_ops[which(tour_ops$id == unique(going)), "investment_ot"],
                                                            max(tour_ops$investment_ot), 10, 0.3))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(satisfaction = ifelse(is.na(going), as.integer(NA),
                               sum(satis_animals, satis_price, satis_wait, satis_infr, satis_invest, na.rm = TRUE))) 
  

# tour operators update rating accroding to tourist satisfaction and clean dataset
tour_ops <- tour_ops %>%
  group_by(id) %>%
  mutate(rating = ifelse(bookings == 0, rating, 
                         mean(c(colMeans(tourists[which(tourists$going == id), "satisfaction"], na.rm = T), rating), na.rm = T)),
         bookings = 0,
         time_with = 0,
         profit = 0) %>%
  ungroup()

# merge tourists back into tourists_pop to keep the waiting counter and sampling probability

tourists_pop$waiting[match(tourists$id, tourists_pop$id)] <- tourists$waiting
tourists_pop$sample_p[match(tourists$id, tourists_pop$id)] <- tourists$sample_p
#}

effects[y] <- tourism_effect(slope_effect, sum(tour_ops$time_with_year), max_times[y])
#}
