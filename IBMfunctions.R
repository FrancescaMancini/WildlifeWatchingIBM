# Functions for wildlife watching IBM ####
# Author: Francesca Mancini
# Date created: 2018-05-15
# Date modified: 2018-05-25

# Wildlife functions ####

# Calculate the effect that tourism had on the population.

# The effect of the time spent with the animals is sigmoid: 
# if less than maxx, effect ~ 0, otherwise it increases. 
# When time is maxx + 1/5maxx, then effect is medium  
# = 0.05 change in probability of encounter.
# Parameter wide determines the range of values over which effect goes from 0 to the maximum (0.1). 
# Lower value of this parameter (i.e. when population is bigger) 
# corresponds to a wider range over which we observe a change.
# Parameter withanimals is the time tour operators spend with animals.
# maxx is the maximum amout of time operators can spend with animals before seeing an effect.

tourism_effect <- function(wide, withanimals, maxx) {
  0.1 + ((- 0.1) / (1 + exp(wide * (sum(withanimals) - (maxx + maxx / 5)))))
}

# Calculate the probability of encounter due to effect on population

# The effect reduced the annual growth rate of the population 
# (here: by affecting the probability of encounter)   

p_encounter <- function(p_e, effect) {
  p_e <- p_e * (1.01 - effect)
  p_e <- ifelse(p_e > 1, 1, p_e)        # probability cannot be > 1
}   


# Calculate new maximum time allowed with animals

time_with_animals <- function(maxx, effect) {
  maxx <- maxx * (1.01 - effect)                          #updates threshold of effect with increase in abundance
  wide <- 0.00025 / (maxx / 100000)
  curve <- c(maxx, wide)                              #updates shape of the curve with increase in abundance
}

# testing wildlife functions
# 
# withanimals <- seq(0, 300000, 1000)
# 
# maxx <- rep(NA, length(withanimals))
# maxx[1] <- 100000
# 
# wide <- rep(NA, length(withanimals))
# wide[1] <- 0.00025 / (maxx[1] / 100000)
# 
# p_e <- rep(NA, length(withanimals))
# p_e[1] <- 0.8
# 
# effect <- rep(NA, length(withanimals))
# 
# for(i in 1:seq_along(withanimals)){
#   effect[i] <- tourism_effect(wide[i], withanimals[i], maxx[i])
#   if(i < length(withanimals)){
#   p_e[i + 1] <- p_encounter(p_e[i], effect[i])
#   curve <- time_with_animals(maxx[i], effect[i])
#   maxx[i + 1] <- curve[1]
#   wide[i + 1] <- curve[2]
#   }
# }
# 
# max_fixed <- 100000
# wide_fixed <- 0.00025
# 
# for(i in 1:seq_along(withanimals)){
#   effect[i] <- tourism_effect(wide_fixed, withanimals[i], max_fixed)
# }
# 
# for(i in 1:seq_along(withanimals)){
#   if(i < length(withanimals)){
#   p_e[i + 1] <- p_encounter(p_e[i], effect[i])
#   }
# }
# 
# for(i in 1:seq_along(withanimals)){
#   if(i < length(withanimals)){
#     curve <- time_with_animals(maxx[i], effect[i])
#     maxx[i + 1] <- curve[1]
#     wide[i + 1] <- curve[2]
#   }
# }

tourists_max <- function(tmax, timeout, tour_ops){
  t_pertour <- tmax/365/dim(tour_ops)[1]                        # calculate n of time allowed with animals per tour
  t_mean <- mean(withanimals, na.rm = T)                        # calculate average time spent with animals per tour in the previous year
  if(t_pertour >= t_mean) {                                     # if time allowed is more or equal the mean
    (sum(tour_ops$capacity)*365) * dim(tour_ops)[1]}            # max tourists is equal to maximum capacity
  else{prop_mean <- (t_mean - t_pertour)/timeout                # othewise calculate the proportion of a trip that needs to be reduced
       n_tours <- (365 - (prop_mean * 365)) * dim(tour_ops)[1]  # transform this proportion into number of tours that can be run
       n_tours * (sum(tour_ops$capacity))}                      # multiply the number of tours by tour operators' capacity to obtain maximum tourists
}

# Tourists functions ####

# From a 2010 survey the number of tourists passing through the Moray Firth dolphin watching locations 
# are 35500 in Peak season and 27500 in off-peak season
# Jan     Feb     Mar     Apr     May     June      July      Aug     Sept      Oct     Nov     Dec
# 315     315    1260    5030    7680     9640      11620    14240    7145     4830     610     315
# Percentage of these that go on a boat tour
# 16%     16%     16%     16%     16%     19%       19%       19%     16%        16%     16%     16%


# Booking tours

booking <- function(tourists, tour_ops){                  # the booking function has 2 arguments, a dataset for tourists and one for tour operators
for(i in seq_len(nrow(tourists))) {                       # loop trhough each tourist
  tourist <- tourists[i, ]
  inbudget <- subset(tour_ops, tour_ops$price <= tourist$price_max)           # extract the subset of tours that are within the budget of the tourist
  preferred <- inbudget[which.max(inbudget$rating), "id"]                     # extract the preferred tour (higher rating)
  if(length(preferred) != 0 &&                                                # if there is a preferred tour and
     tour_ops[preferred, "rating"] > tourist["rating_min"] &&         # if the rating of this tour is higher than the the tourist's minimum rating and
     tour_ops[preferred, "capacity"] > tour_ops[preferred, "bookings"]) {     # if the tour operator is not fully booked
    tour_ops[preferred, "bookings"] <- tour_ops[preferred, "bookings"] + 1    # add a booking to the preferred tour operator
    tourists[i, "going"] <- "Yes"                                             # the tourist is going on the tour
  } else {inbudget <- subset(inbudget, inbudget$id != preferred)              # otherwise delete the preferred tour from the inbudget vector and
  while(nrow(inbudget) != 0 &&                                                # for as long as there are tours in the inbudget vector and
        length(inbudget$rating[inbudget$rating > tourist$rating_min]) > 0) {  # there are tours with higher rating than the tourist's minimum
    preferred <- as.numeric(inbudget[which.max(inbudget$rating), "id"])       # select the second preferred tour and
    if (length(preferred) != 0 &&                                             # if all conditions are met
        tour_ops[preferred, "rating"] > tourist["rating_min"] &&
        tour_ops[preferred, "capacity"] > tour_ops[preferred, "bookings"]) {
      tour_ops[preferred, "bookings"] <- tour_ops[preferred, "bookings"] + 1  # add a booking to the tour and
      tourists[i, "going"] <- "Yes"                                           # the tourist is going on the tour
      break
    } else {
      inbudget <- subset(inbudget, inbudget$id != preferred)                  # otherwise try the third preferred tour and then the fourth etc...
    }
  }
  if(tourists[which(tourists$id == tourist$id), "going"] == "") {             # if the tourist has not found a tour 
    tourists[which(tourists$id == tourist$id), "going"] <- "No"               # the tourist is not going on any tour and
    tourists[which(tourists$id == tourist$id), "waiting"] <- tourists[tourist$id, "waiting"] + 1      # add 1 day to the tourists's waiting time
  }
  }
}
  invisible(list(tourists,tour_ops))                                          # return the two dataframes in a list
}


# Testing

# create dataframes to hold preferences for tourists and caracteristics for tour operators

# tour_ops <- data.frame(id = seq(1, 10, 1), price = rnorm(10, 15, 5), rating = rnorm(10, 3, 1.5),
#                       capacity = as.integer(runif(10, 10, 30)), bookings = rep(0, 10))
#  
# tourists <- data.frame(id = seq(1, 1000, 1), price_max = rnorm(1000, 15, 5),
#                        rating_min = rnorm(1000, 3, 0.5), going = character(length = 1000), waiting = rep(0, 1000), stringsAsFactors=FALSE)


# run the function 

# bookings <- booking(tourists, tour_ops)

# extract dataframes from list
# tourists <- bookings[[1]]
# tour_ops <- bookings[[2]]


# Satisfaction

# tourist satisfaction is affected by different caracteristics: 
# time spent with the animals, price/quality ratio, waiting time for booking,
# investment in infrastructure and other services

# satisfaction is calculated as a sigmoid curve

# the proportion of tour time spent with animals determines tourist satisfaction
# with an inflectioon point of 0.3 and a variable slope. 

satisfaction_animals <- function(withanimals, timeout, slope) {   
  1 + (0.01 - 1) / (1 + exp(slope * ((withanimals / timeout) - 0.3)))}

# testing

# withanimals <- seq(10, 180, 10)
# timeout <- 180
# 
# satisfaction <- satisfaction_animals(withanimals, timeout, 15)
# 
# plot(satisfaction~withanimals)

# satisfaction regarding price/quality ratio is also expressed as 
# a sigmoid relationship.

satisfaction_price <- function(price, rating, slope, infl) {
  1 / (1 + exp(-slope * (1 / (price / rating) - infl)))
}
  
# testing
# price <- 10
# rating <- seq(1, 5, 0.1)
# 
# satisfaction <- satisfaction_price(price, rating, slope = 10, infl = 0.3)
# plot(satisfaction~rating)


# the proportion of the year a tourist had to wait before being able
# to book a tour determines their satisfaction in a similar way

satisfaction_waiting <- function(waiting, slope, infl) {
  1 / (1 + exp(slope * ((waiting / 365) - infl)))
}

# testing

# waiting <- seq(1, 365, 1)
# satisfaction <- satisfaction_waiting(waiting, 10,0.3)
# plot(satisfaction~waiting)


# the proportion of tour operator's profits that is reinvested into 
# infrastructure (here) or other services (below) influences tourist satisfaction

satisfaction_infr_investment <- function(infr_investment, slope, infl){
 1 / (1 + exp(-slope * ((infr_investment / profit) - infl)))
}

# testing

# investment <- seq(0, 10000, 100)
# profit <- 10000
# satisfaction <- satisfaction_infr_investment(investment, 10, 0.1)
# prop_investment <- investment/profit
# plot(satisfaction ~ prop_investment)


satisfaction_other_investment <- function(investment_other, slope, infl){
 1 / (1 + exp(-slope * ((investment_other / profit) - infl)))
}


# Tour operators ####

# Tour operators can modify the price of their tours on a yearly basis.
# the change in the price of the ticket is dependent only on any
# extra investment the tour operator decided to make at the end of the previous year

# price_change  
# deltaP = ((deltaQ/Q)/PE) * P

price_change <- function(extra_cost, ntours){
  extra_cost / ntours}

