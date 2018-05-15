# Functions for wildlife watching IBM ####
# Author: Francesca Mancini
# Date created: 2018-05-15
# Date modified: 

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
  0.1 + (0 - 0.1) / (1 + exp(wide * (sum(withanimals) - (maxx + maxx / 5))))
}

# Calculate the probability of encounter due to effect on population

# The effect reduced the annual growth rate of the population 
# (here: by affecting the probability of encounter)   

p_encounter <- function(p_e, effect) {
  p_e * (1.01 - effect)
  p_e <- ifelse(p_e > 1, 1, p_e)        # probability cannot be > 1
}   



