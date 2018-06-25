# IBM scenario 1: Code of Conduct ####
# Author: Francesca Mancini
# Date created: 2018-05-30
# Date modified: 2018-06-25

library(dplyr)
source("IBMfunctions.R")

# initialise population of tourists and tour operators ####
set.seed(123)

# years months and days
years <- 3
# months <- 12
days <- 365

# fine
#fine <- 1000

# create vector of behavioural phenotypes for tour operators

phenotypes <- c("trustful", "optimist", "pessimist", "envious", "undefined")

# the phenotypes will be sampled from a population with the following proportions of phenotypes
# Poncela-Casasnovas et al, 2016. Science Advances 2(8): e1600451-e1600451. doi:10.1126/sciadv.1600451

p_trustful <- 0.17
p_optimist <- 0.2
p_pessimist <- 0.21
p_envious <- 0.3
p_undefined <- 0.12


tour_ops <- data.frame(id = seq(1, 10, 1), price = rnorm(10, 30, 1), rating = rep(2.5, 10),
                      capacity = as.integer(runif(10, 10, 30)), bookings = rep(0, 10), bookings_year =  rep(0, 10),
                      investment_infra = rep(0.001, 10), investment_ot = rep(0.001, 10), 
                      time_with = rep(0, 10), time_with_year = rep(0, 10), 
                      profit = rep(0, 10), profit_year = rep(0, 10),
                      phenotype = sample(phenotypes, 10, replace=TRUE, 
                                         prob=c(p_trustful, p_optimist, p_pessimist, p_envious, p_undefined)),
                      behaviour = character(10), tours = rep(365, 10), stringsAsFactors = FALSE)

capacity_0 <- sum(tour_ops$capacity)

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
encounter_probs[1] <- 0.5

# time_allowed <- rep(NA, years)
# 
# tourists_allowed <- rep(NA, years)

max_times <- rep(NA, years)
max_times[1] <- 100000

wide <- 0.00025 / (max_times[1] / 100000)


profits <- vector("list", years)
investments <- vector("list", years)
bookings_year <- vector("list", years)
ratings_year <- vector("list", years)
prices <- vector("list", years)
withanimals <- vector("list", years)

for(y in 1:years){                                  # start year loop
  
# create year tourists population
tourists_pop <- data.frame(id = seq(1, 1000000, 1), price_max = c(rnorm(60000, 30, 1), rnorm(30000, 45, 1), rnorm(10000, 60, 1)),
                       rating_min = runif(1000000, 2, 3.5), going = rep(NA, 1000000), 
                       waiting = rep(0, 1000000), sample_p = rep(0.5, 1000000), 
                       satisfaction = rep(NA, 1000000), satis_random = rep(NA, 1000000),
                       satis_animals = rep(NA, 1000000), satis_price = rep(NA, 1000000), 
                       satis_invest = rep(NA, 1000000), satis_wait = rep(NA, 1000000), 
                       stringsAsFactors=FALSE)

# calculates encounter probabilities 
# and maximum time that can be sustainably be spent with animals
# according to effect of tourism in the previous year

max_times[y] <- ifelse(y == 1, max_times[y], time_with_animals(maxx = max_times[y-1], effect = effects[y-1])) 

encounter_probs[y] <- ifelse(y == 1, encounter_probs[y], p_encounter(p_e = encounter_probs[y-1], effect = effects[y-1]))

# create dataset to store daily ratings

ratings <- vector("list", days)

for(d in 1:days){


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

# add daily bookings to year bookings

tour_ops$bookings_year <- tour_ops$bookings_year + tour_ops$bookings

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
         satis_invest = ifelse(is.na(going), as.integer(NA),
                              satisfaction_other_investment(tour_ops[which(tour_ops$id == unique(going)), "investment_ot"],
                                                            max(tour_ops$investment_ot), 10, 0.3)),
         satis_random = ifelse(is.na(going), as.integer(NA),
                               rbinom(1, 1, p = (satis_animals * satis_price * satis_wait * satis_invest)))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(satisfaction = ifelse(is.na(going), as.integer(NA),
                               ifelse(satis_random == 1,
                                      sum(satis_animals, satis_price, satis_wait, satis_invest, na.rm = TRUE) + 
                                        (satis_animals * satis_price * satis_wait * satis_invest),
                                      sum(satis_animals, satis_price, satis_wait, satis_invest, na.rm = TRUE) - 
                                        (satis_animals * satis_price * satis_wait * satis_invest))))

  

# tour operators update rating accroding to tourist satisfaction and clean dataset
tour_ops <- tour_ops %>%
  group_by(id) %>%
  mutate(rating = ifelse(bookings == 0, rating, 
                         median(c(colMeans(tourists[which(tourists$going == id), "satisfaction"], na.rm = T), rating), na.rm = T)),
         bookings = 0,
         time_with = 0,
         profit = 0) %>%
  ungroup()

# store daily ratings
ratings[[d]] <- data.frame(id=tour_ops$id, rating=tour_ops$rating)

# merge tourists back into tourists_pop to keep the waiting counter and sampling probability

tourists_pop$waiting[match(tourists$id, tourists_pop$id)] <- tourists$waiting
tourists_pop$sample_p[match(tourists$id, tourists_pop$id)] <- tourists$sample_p
}

# calculate and store median rating and standard deviation

ratings_year[[y]] <- data.frame(id = tour_ops$id, year=rep(y,length(tour_ops$id)), 
                                rating = aggregate(rating ~ id, data = do.call(rbind, ratings), median)[,2],
                                SD = aggregate(rating ~ id, data = do.call(rbind, ratings), sd)[,2])

# calculate tourism effect in the past year
wide_time <- 0.00025/(max_times[y]/100000)
wide_capacity <- 0.2 / (max_times[y]/100000)

effects[y] <- tourism_effect(slope_time = wide_time, slope_capacity = wide_capacity, 
                             init_capacity = capacity_0, new_capacity = sum(tour_ops$capacity),
                             withanimals = sum(tour_ops$time_with_year), maxx = max_times[y])    

# decide on infrastruucture investment

tour_ops <- tour_ops %>%
  mutate(investment_ot = invest_services(rating = rating, max_rating = max(rating), profit = profit_year),
         investment_infra = invest_infrastructure(profit = profit_year, max_profit = capacity * price * 365, capacity = capacity, ticket = price)) %>%
  mutate(investment_infra = ifelse((profit_year - 35000) - (investment_infra + investment_ot) > 0, investment_infra, 0.001),
         capacity = ifelse(investment_infra > 0.001, capacity + as.integer((profit - 35000) / (capacity * price * 14)), capacity))

# store tour operators investments in a list
investments[[y]] <- data.frame(id = tour_ops$id, year=rep(y,length(tour_ops$id)), 
                               infrastructure = tour_ops$investment_infra, 
                               services = tour_ops$investment_ot)

# store tour operators annual profits in a list
profits[[y]] <- data.frame(id=tour_ops$id, year=rep(y,length(tour_ops$id)), money=tour_ops$profit_year)
# remeber to use
# profits <- do.call("rbind", profits)
# to transform into data.frame

# store tour operators bookings in a list
bookings_year[[y]] <- data.frame(id=tour_ops$id, year=rep(y,length(tour_ops$id)), bookings = tour_ops$bookings_year)

# store prices
prices[[y]] <- data.frame(id=tour_ops$id, year=rep(y,length(tour_ops$id)), ticket_price = tour_ops$price)

# store time spent with animals
withanimals[[y]] <- data.frame(id=tour_ops$id, year=rep(y,length(tour_ops$id)), time = tour_ops$time_with_year)

# operators who have had no profits for the past 3 years retire
if(y > 2) {bankrupt <- as.numeric(names(which(table(do.call("rbind", lapply(profits[(y-3):y], subset, money == 0))$id) == 3)))
           tour_ops <- subset(tour_ops, !(id %in% bankrupt))}

# tour operators update prices according to costs

tour_ops <- tour_ops %>%
  mutate(price = case_when(price_change(ticket = tour_ops$price, demand = sum(tour_ops$bookings_year, na.rm = T), supply = sum(tour_ops$capacity) * 365) > (0.7 * 90) / tour_ops$capacity ~ price_change(ticket = price, demand = sum(tour_ops$bookings_year, na.rm = T), supply = sum(tour_ops$capacity) *365),   
                           TRUE ~ price))


# new operator start?
# probability of new operators wanting to start given by demand / supply ratio
# probability is used in a binomial draw
p_to <- sum(tour_ops$bookings_year, na.rm = T) / (sum(tour_ops$capacity) * 365)
new_tour_ops <- rbinom(1, 1, p = ifelse(p_to > 1, 1, p_to))

# if binomial draw is one, one new TO starts in the next year
if(new_tour_ops == 1) {tour_ops <- rbind(tour_ops, data.frame(id = max(tour_ops$id) + 1, price = runif(1, 15, 25), rating = runif(1, 3, 5),
                      capacity = as.integer(runif(1, 10, 30)), bookings = 0, bookings_year =  0, investment_infra = 0.001, 
                      investment_ot = 0.001,time_with = 0, 
                      time_with_year = 0, profit = 0, profit_year = 0,  phenotype = sample(phenotypes, 1, replace=TRUE, 
                                         prob=c(p_trustful, p_optimist, p_pessimist, p_envious, p_undefined)),
                      behaviour = character(1), tours = 365, stringsAsFactors = FALSE))}

# set profits and time with animals back to 0
tour_ops$profit_year <- 0
tour_ops$time_with_year <- 0

# keep track of simulation
print(y)
}


