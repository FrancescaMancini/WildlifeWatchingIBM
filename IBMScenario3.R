# IBM scenario 3: User group management ####
# Author: Francesca Mancini
# Date created: 2018-06-25
# Date modified: 2018-07-20

# Scenario 3 is the user group management strategy.
# Tour operators have proporty rights over the widlife 
# They conduct monitoring of the wildife but do not enforce fines
# The tour operators have tradable wildlife allowances
# A minute with the animals costs 2 units and at the end of the day
# they can decide if they want to sell some of their unused
# allowance or buy some more time


library(doParallel)
library(foreach)



# initialise population of tourists and tour operators ####
set.seed(123)


# create dataframe of parameters
params <- data.frame(trends = rep(c(0.005, 0, -0.005), each = 3), 
                     min_rating_shape1 = rep(c(0.8, 3, 1.9), 3), 
                     min_rating_shape2 = rep(c(3, 0.8, 1.9), 3))

cl <- makeCluster(9)

registerDoParallel(cl)

results <- foreach(i = 1:9, .packages = c("dplyr", "RGeode")) %dopar% {

source("IBMfunctionsS3.R")
  
# years and days
years <- 100
days <- 365

# cost of time with animals per minute
twa_cost <- 2

# create vectors to store wildlife and management parameters

effects <- rep(NA, years)

encounter_probs <- rep(NA, years)
encounter_probs[1] <- 0.5
max_times <- rep(NA, years)
max_times[1] <- 100000


profits <- vector("list", years)
investments <- vector("list", years)
bookings_year <- vector("list", years)
ratings_year <- vector("list", years)
prices <- vector("list", years)
withanimals <- vector("list", years)
behaviours_year <- vector("list", years)



# create vector of behavioural phenotypes for tour operators

phenotypes <- c("trustful", "optimist", "pessimist", "envious", "undefined")

# the phenotypes will be sampled from a population with the following proportions of phenotypes
# Poncela-Casasnovas et al, 2016. Science Advances 2(8): e1600451-e1600451. doi:10.1126/sciadv.1600451

p_trustful <- 0.17
p_optimist <- 0.2
p_pessimist <- 0.21
p_envious <- 0.3
p_undefined <- 0.12


tour_ops <- data.frame(id = seq(1, 10, 1), price = rnorm(10, 30, 1), rating = rep(3, 10),
                      capacity = as.integer(runif(10, 10, 30)), bookings = rep(0, 10), bookings_year =  rep(0, 10),
                      investment_infra = rep(0.001, 10), investment_ot = rep(0.001, 10), 
                      time_with = rep(0, 10), time_with_year = rep(0, 10), 
                      profit = rep(0, 10), profit_year = rep(0, 10),
                      phenotype = sample(phenotypes, 10, replace=TRUE, 
                                         prob=c(p_trustful, p_optimist, p_pessimist, p_envious, p_undefined)),
                      behaviour = character(10), t_allowed = rep(max_times[1]/10/days, 10), 
                      TWA = character(10), stringsAsFactors = FALSE)

tour_ops$behaviour <- factor(tour_ops$behaviour, levels = c("defect", "cooperate", "no choice"))

capacity_0 <- sum(tour_ops$capacity)

# generate daily time series of tourists
tourists_start_mean <- 200
days_tot <- years* days  # number of days in total

#effect sizes
y_effect <- params[i, "trends"]           # trends in demand
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

n_tourists <- ifelse(n_tourists > 0, n_tourists, 0)


for(y in 1:years){                                  # start year loop
  
# create year tourists population
tourists_pop <- data.frame(id = seq(1, 1000000, 1), price_max = c(rnorm(60000, 30, 1), rnorm(30000, 45, 1), rnorm(10000, 60, 1)),
                       rating_min = rbeta(1000000, params[i, "min_rating_shape1"], params[i, "min_rating_shape2"]) * 5, going = rep(NA, 1000000),
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

# create a list to store daily ratings

ratings <- vector("list", days)

# create a list to store daily behaviours

behaviours <- vector("list", days)


for(d in 1:days){


day_of_sim <- d + (y - 1) * days

### daily tourists here

tourists <- tourists_pop[sample(nrow(tourists_pop), n_tourists[day_of_sim], replace = FALSE, prob = tourists_pop$sample_p), ]

if(nrow(tourists) == 0){
  next
  }

bookings <- booking(tourists, as.data.frame(tour_ops))

# extract dataframes from list
tourists <- bookings[[1]]
tour_ops <- bookings[[2]]

# avoid tour operators' profits being negative
# if booking * price - costs < 0 then tour operator does not run tour
tour_ops$bookings <- ifelse(((tour_ops$bookings * tour_ops$price) - (1.5 * 90)) < 0, 0, tour_ops$bookings)

tourists[which(tourists$going %in% which(tour_ops$bookings == 0)), c("going", "waiting")] <-  c(NA, +1)

# add daily bookings to year bookings

tour_ops$bookings_year <- tour_ops$bookings_year + tour_ops$bookings

# calculate time spent with animals
tour_ops <- tour_ops %>%
  mutate(time_with = ifelse(bookings > 0, encounter_time(n_ops = length(tour_ops$id), p_e = encounter_probs[y]), 0))

# choose behavioural strategy (cooperate or defect).
# If the time calculated above is more than what is allowed
# the tour operator makes a choice according to costs and benefits

tour_ops  <- behaviour_choice(tour_ops = tour_ops, payoff_CC = CC.3(tour_ops$price, tour_ops$capacity, tour_ops$t_allowed, 90, 15, length(tour_ops$id)),
                      payoff_CD = CD.3(tour_ops$price, tour_ops$capacity, tour_ops$t_allowed, tour_ops$time_with, 90, 15, length(tour_ops$id)),
                      payoff_DC = DC.3(tour_ops$price, tour_ops$capacity, tour_ops$time_with, tour_ops$t_allowed, 90, 15, length(tour_ops$id)),
                      payoff_DD = DD.3(tour_ops$price, tour_ops$capacity, tour_ops$time_with, 90, 15), tour_ops$t_allowed)

tour_ops$behaviour <- factor(tour_ops$behaviour, levels = c("defect", "cooperate", "no choice"))

# if tour operator cooperates time_with is the maximum allowed
# if defects time_with is the time calculated above
tour_ops <- tour_ops %>%
  mutate(time_with = case_when(bookings == 0 ~ 0,
                               behaviour == "defect" | behaviour == "no choice" ~ time_with,
                               behaviour == "cooperate" ~ tour_ops$t_allowed))

# tour operators update time spent with animals in the year and profits
tour_ops <- tour_ops %>%
  mutate(time_with_year = time_with_year + time_with,
    profit = case_when(bookings == 0 ~ 0, 
                       TRUE ~ (bookings * price) - (1.5 * 90)))
    

# Tradable wildlife allowance
# if a tour operator has spent more than 79% of their allowed time with the animals
# they will try to buy more time from those tour operators who have spent less than a third of their allowance
# The time allowances sold by the tour operators go into a pot and then are distributed 
# among the buying tour operators

tour_ops <- tour_ops %>%
  mutate(TWA = case_when(time_with == 0 ~ as.character(NA),
                   time_with >= 0.8 * t_allowed ~ "buy",
                   time_with < t_allowed/3 ~ "sell",
                   TRUE ~ as.character(NA))) 

if("buy" %in% tour_ops$TWA == T && "sell" %in% tour_ops$TWA == T){
  allowances <- sum(tour_ops[which(tour_ops$TWA == "sell"), "t_allowed"] - tour_ops[which(tour_ops$TWA == "sell"), "time_with"])
  tour_ops <- tour_ops %>%
    group_by(TWA) %>%
    mutate(profit = case_when(is.na(TWA) ~ profit,
                               TWA == "sell" ~ profit + ((t_allowed - time_with) * twa_cost),
                               TWA == "buy" ~ profit - ((allowances/sum(tour_ops$TWA == "buy", na.rm = T)) * twa_cost),
                               TRUE ~ profit),
           t_allowed = case_when(is.na(TWA) ~ max_times[y]/10/days,
                                 TWA == "sell" ~ max_times[y]/10/days - (t_allowed - time_with),
                                 TWA == "buy" ~ max_times[y]/10/days + (allowances/sum(tour_ops$TWA == "buy", na.rm = T)),
                                 TRUE ~ max_times[y]/10/days)) %>%
    ungroup()
}

# update annual profits

tour_ops <- tour_ops  %>%
    mutate(profit_year = profit_year + profit)
 
tour_ops <- as.data.frame(tour_ops)
tourists <- as.data.frame(tourists)

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
         profit = 0,
         TWA = as.character(NA)) %>%
  ungroup()

# store daily ratings
ratings[[d]] <- data.frame(id=tour_ops$id, rating=tour_ops$rating)

# store daily behaviours
behaviours[[d]] <- data.frame(id = tour_ops$id, behaviour = tour_ops$behaviour)

# merge tourists back into tourists_pop to keep the waiting counter and sampling probability

tourists_pop$waiting[match(tourists$id, tourists_pop$id)] <- tourists$waiting
tourists_pop$sample_p[match(tourists$id, tourists_pop$id)] <- tourists$sample_p
}

# calculate and store median rating and standard deviation

ratings_year[[y]] <- data.frame(id = tour_ops$id, year=rep(y,length(tour_ops$id)), 
                                rating = aggregate(rating ~ id, data = do.call(rbind, ratings), median)[,2],
                                SD = aggregate(rating ~ id, data = do.call(rbind, ratings), sd)[,2])

# calculate and store daily behaviours of tour operators

behaviours_year[[y]] <- data.frame(id = tour_ops$id, year=rep(y,length(tour_ops$id)),
                                   defect = aggregate(behaviour ~ id, data = do.call(rbind, behaviours), table)$behaviour[,"defect"],
                                   cooperate = aggregate(behaviour ~ id, data = do.call(rbind, behaviours), table)$behaviour[,"cooperate"])


# calculate tourism effect in the past year
wide_time <- 0.00025/(max_times[y]/100000)
wide_capacity <- 0.2 / (max_times[y]/100000)

effects[y] <- tourism_effect(slope_time = wide_time, slope_capacity = wide_capacity, 
                             init_capacity = capacity_0, new_capacity = sum(tour_ops$capacity),
                             withanimals = sum(tour_ops$time_with_year), maxx = max_times[y])    

# decide on infrastruucture investment

tour_ops <- tour_ops %>%
  mutate(investment_ot = invest_services(rating = rating, max_rating = max(rating), profit = profit_year),
         investment_infra = invest_infrastructure(profit = profit_year, max_profit = capacity * price * days, capacity = capacity, ticket = price)) %>%
  mutate(investment_infra = ifelse(profit_year - (investment_infra + investment_ot) > 0, investment_infra, 0.001),
         capacity = ifelse(investment_infra > 0.001, capacity + as.integer(profit / (capacity * price * 14)), capacity))

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

if(nrow(tour_ops) == 0){break}


# tour operators update prices according to costs

tour_ops <- tour_ops %>%
  mutate(price = case_when(price_change(ticket = tour_ops$price, demand = sum(tour_ops$bookings_year, na.rm = T), supply = sum(tour_ops$capacity) * days) > (1.5 * 90) / tour_ops$capacity ~ price_change(ticket = price, demand = sum(tour_ops$bookings_year, na.rm = T), supply = sum(tour_ops$capacity) *days),   
                           TRUE ~ price))


# new operator start? Every 6 years new TOs can start
# probability of new operators wanting to start given by demand / supply ratio
# probability is used in a binomial draw
if(y %in% seq(6, years, 6)) {
p_to <- sum(tour_ops$bookings_year, na.rm = T) / (sum(tour_ops$capacity) * days)
new_tour_ops <- rbinom(1, 1, p = ifelse(p_to > 1, 1, p_to))} 
else{new_tour_ops <- 0}

# if binomial draw is one 
# and if all tour operators agree (multiple binomial draw with probabilities equal to 
# the individual tour operator's demand/supply ratio) a new operator starts
if(new_tour_ops == 1 &&
   all(rbinom(dim(tour_ops)[1], 1, prob = tour_ops$bookings_year/(tour_ops$capacity * days)) == 1)) {
  tour_ops <- rbind(tour_ops, data.frame(id = max(tour_ops$id) + 1, price = rnorm(1, mean(tour_ops$price), 1), rating = 3,
                                         capacity = as.integer(runif(1, 10, 30)), bookings = 0, bookings_year =  0, investment_infra = 0.001, 
                                         investment_ot = 0.001,time_with = 0, 
                                         time_with_year = 0, profit = 0, profit_year = 0,  phenotype = sample(phenotypes, 1, replace=TRUE, 
                                         prob=c(p_trustful, p_optimist, p_pessimist, p_envious, p_undefined)),
                                         behaviour = character(1), t_allowed = rep(max_times[1]/10/days, 10), 
                                         TWA = character(10), stringsAsFactors = FALSE))}

# set profits bookings and time with animals back to 0
tour_ops$profit_year <- 0
tour_ops$time_with_year <- 0
tour_ops$bookings_year <- 0

# keep track of simulation
print(y)
}
list(effect = effects, e_probs = encounter_probs, time_max = max_times, 
     income = profits, invest = investments, bookings = bookings_year, 
     ratings = ratings_year, tickets = prices, time_with = withanimals, behaviours = behaviours_year)
}

stopCluster(cl)
