# Visualisations ####
# Author: Francesca Mancini
# Date created: 2018-06-27
# Date modified: 2018-09-14

library(ggplot2)
library(plyr)
library(ggridges)
library(viridis)
library(data.table)


# Read scenario file
Scenario1 <- readRDS("../SimResults/Scenario1.rds")
Scenario2 <- readRDS("../SimResults/Scenario2.rds")
Scenario3 <- readRDS("../SimResults/Scenario3.rds")
Scenario4 <- readRDS("../SimResults/Scenario4.rds")

# Scenario3.1 <- readRDS("../SimResults/Scenario3.1.rds")
# Scenario4.1 <- readRDS("../SimResults/Scenario4.1.rds")

unpack_list <- function(scenario, m, behaviour){
name <- c("experiment1.1", "experiment1.2", "experiment1.3", "experiment1.4", "experiment1.5",
          "experiment2.1", "experiment2.2", "experiment2.3", "experiment2.4", "experiment2.5", 
          "experiment3.1", "experiment3.2", "experiment3.3", "experiment3.4", "experiment3.5",
          "experiment4.1", "experiment4.2", "experiment4.3", "experiment4.4", "experiment4.5", 
          "experiment5.1", "experiment5.2", "experiment5.3", "experiment5.4", "experiment5.5", 
          "experiment6.1", "experiment6.2", "experiment6.3", "experiment6.4", "experiment6.5", 
          "experiment7.1", "experiment7.2", "experiment7.3", "experiment7.4", "experiment7.5", 
          "experiment8.1", "experiment8.2", "experiment8.3", "experiment8.4", "experiment8.5", 
          "experiment9.1", "experiment9.2", "experiment9.3", "experiment9.4", "experiment9.5")
params <- data.frame(trends = rep(c(0.005, 0, -0.005), each = 15), tourists = rep(c("generalists", "specialists", "mixed"), each = 5))
experiments <- rep(seq(1, 9, 1), each = 5)
iter <- rep(seq(1, 5, 1), 9)
management <- c("Code of conduct", "Licensing", "User group", "Co-management", "User group - no TWQ", "CO-management - no TWQ")
Scenario1_socioecon <- NULL
Scenario1_ecol <- NULL

for(i in seq_along(name)){
  assign(name[i], scenario[[i]])
  
  assign(paste("exp", experiments[i], ".", iter[i], "_socioecon", sep = ""), 
         join_all(list(do.call("rbind", get(ls()[grep("experiment", ls())][i])$bookings),
                       do.call("rbind", get(ls()[grep("experiment", ls())][i])$invest), 
                       do.call("rbind", get(ls()[grep("experiment", ls())][i])$tickets), 
                       do.call("rbind", get(ls()[grep("experiment", ls())][i])$income), 
                       do.call("rbind", get(ls()[grep("experiment", ls())][i])$ratings), 
                       do.call("rbind", get(ls()[grep("experiment", ls())][i])$time_with)), 
                  by = c("id", "year"), type = "full"))
  
  if(behaviour == TRUE){
    assign(paste("exp", experiments[i], ".", iter[i], "_socioecon", sep = ""), 
           join_all(list(get(ls()[grep("socioecon", ls())][i]), 
                         do.call("rbind", get(ls()[grep("experiment", ls())][i])$behaviours))))
  }else{assign(paste("exp", experiments[i], ".", iter[i], "_socioecon", sep = ""), cbind(get(ls()[grep("socioecon", ls())][i]), 
                                                 defect = rep(NA, dim(get(ls()[grep("socioecon", ls())][i]))[1]),
                                                 cooperate = rep(NA, dim(get(ls()[grep("socioecon", ls())][i]))[1])))}
  
  assign(paste("exp", experiments[i], ".", iter[i], "_socioecon", sep = ""), cbind(get(ls()[grep("socioecon", ls())][i]), 
                                                 trends = rep(params[i, "trends"], dim(get(ls()[grep("socioecon", ls())][i]))[1]),
                                                  tourists = rep(params[i, "tourists"], dim(get(ls()[grep("socioecon", ls())][i]))[1]),
                                                 management = rep(management[m], dim(get(ls()[grep("socioecon", ls())][i]))[1]),
                                                 iteration = iter[i]))
  
  assign(paste("exp", experiments[i], ".", iter[i], "ecol", sep = ""), 
         data.frame(management = rep(management[m], 50), trends = rep(params[i, "trends"], 50), 
                    tourists = rep(params[i, "tourists"], 50), 
                    iteration = iter[i], year = seq(1, 50, 1),
                    effect = get(ls()[grep("experiment", ls())][i])$effect, 
                    p_encounter = get(ls()[grep("experiment", ls())][i])$e_probs, 
                    time_max = get(ls()[grep("experiment", ls())][i])$time_max))

  Scenario1_socioecon <- rbind(Scenario1_socioecon, get(ls()[grep("socioecon", ls())][i]))
  Scenario1_ecol <- rbind(Scenario1_ecol, get(ls()[grep("ecol", ls())][i]))
}
  invisible(list(Scenario1_socioecon, Scenario1_ecol))
}

Scenario1_df <- unpack_list(Scenario1, 1, behaviour = FALSE)

Scenario1_socioecon <- Scenario1_df[[1]]
Scenario1_ecol <- Scenario1_df[[2]]


Scenario2_df <- unpack_list(Scenario2, 2, behaviour = TRUE)

Scenario2_socioecon <- Scenario2_df[[1]]
Scenario2_ecol <- Scenario2_df[[2]]


Scenario3_df <- unpack_list(Scenario3, 3, behaviour = TRUE)

Scenario3_socioecon <- Scenario3_df[[1]]
Scenario3_ecol <- Scenario3_df[[2]]

Scenario4_df <- unpack_list(Scenario4, 4, behaviour = TRUE)

Scenario4_socioecon <- Scenario4_df[[1]]
Scenario4_ecol <- Scenario4_df[[2]]

# Scenario3.1_df <- unpack_list(Scenario3.1, 5, behaviour = TRUE)
# 
# Scenario3.1_socioecon <- Scenario3.1_df[[1]]
# Scenario3.1_ecol <- Scenario3.1_df[[2]]
# 
# Scenario4.1_df <- unpack_list(Scenario4.1, 6, behaviour = TRUE)
# 
# Scenario4.1_socioecon <- Scenario4.1_df[[1]]
# Scenario4.1_ecol <- Scenario4.1_df[[2]]



#### All sims ####

pal <- viridis(3, begin = 0, end = 0.9)

all_sims_socioecon <- rbind(Scenario1_socioecon, Scenario2_socioecon, Scenario3_socioecon, 
                            Scenario4_socioecon
                            #, Scenario3.1_socioecon, Scenario4.1_socioecon
                            )

all_sims_ecol <- rbind(Scenario1_ecol, Scenario2_ecol, Scenario3_ecol, 
                       Scenario4_ecol
                       #, Scenario3.1_ecol, Scenario4.1_ecol
                       )

# how long do tour operators stay in business?

length_business <- count(all_sims_socioecon, c("id", "trends", "tourists", "management", "iteration"))

ggplot(data = length_business) +
  geom_jitter(aes(x = tourists, y = freq), shape = 20, color = "black", size = 1) +
  geom_violin(aes(x = tourists, y = freq, fill = tourists), alpha = 0.8) +
  stat_summary(aes(x = tourists, y = freq), fun.y="median", geom="point") +
  scale_fill_manual(values = pal, name = "Type of tourists") +
  ylab("Years in business") +
  xlab("Type of tourists") +
  facet_wrap(~ management + trends, ncol = 3, nrow = 6)  +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        text=element_text(size=12), panel.grid.major = element_blank(),                           
        panel.background = element_blank())


# how many new tour operators start?
n_tourops_new <- setDT(subset(all_sims_socioecon, id > 10))[, .(count = uniqueN(id)), by = c("trends", "tourists", "management", "year")]

tour_ops_new <- ggplot(data = n_tourops_new, aes(x = year, y = count)) +
 geom_point(aes(colour = tourists), alpha = 0.2) +
 geom_smooth(aes(colour = tourists, group = tourists)) +
 scale_color_manual(values = pal) +
 facet_wrap(~ management + trends, ncol = 3, nrow = 6)

tour_ops_new


# A look at the final state of the industry in the last 10 years

all_sims_socioecon <- subset(all_sims_socioecon, year > 40)
all_sims_ecol <- subset(all_sims_ecol, year > 40)

# visualise time series of number of bookings per tour operator

books <- ggplot(data = aggregate(bookings ~ year + management + trends + tourists + iteration, 
                                 data = all_sims_socioecon, mean, na.rm=FALSE), 
                aes(x = year, y = bookings)) +
 geom_point(aes(colour = tourists), alpha = 0.7) +
 geom_smooth(aes(x = year, y = bookings, colour = tourists, group = tourists)) +
 scale_color_manual(values = pal, name = "Type of tourists") +
 scale_x_continuous(name = "Year", breaks = c(40, 43, 46, 49), position = "bottom") +
 scale_y_continuous(name = "N bookings", position = "left") +
 facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
 theme(text=element_text(size=12),                           
       panel.background = element_blank())

books

ggplot(aggregate(bookings ~ year + management + trends + tourists + iteration, 
                                 data = all_sims_socioecon, mean, na.rm=FALSE), 
                aes(x = bookings, y = tourists)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
  scale_fill_viridis(name = "Bookings\n") +
  facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
  ylab("Type of tourists") +
  xlab("N bookings") +
  theme_ridges() +
  theme(strip.text.x = element_text(size = 12))


# visualising investments
invest <- ggplot(data = aggregate(infrastructure ~ year + management + trends + tourists + iteration, 
                                 data = all_sims_socioecon, mean, na.rm=FALSE), 
                aes(x = year, y = infrastructure)) +
 geom_point(aes(colour = tourists), alpha = 0.7) +
 geom_smooth(aes(x = year, y = infrastructure, colour = tourists, group = tourists)) +
 scale_color_manual(values = pal, name = "Type of tourists") +
 scale_x_continuous(name = "Year", breaks = c(40, 43, 46, 49), position = "bottom") +
 scale_y_continuous(name = "Infrastructure investment", position = "left") +
 facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
 theme(text=element_text(size=12),                           
       panel.background = element_blank())
invest

ggplot(aggregate(infrastructure ~ year + management + trends + tourists + iteration, 
                                 data = all_sims_socioecon, mean, na.rm=FALSE), 
                aes(x = infrastructure, y = tourists)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
  scale_fill_viridis(name = "Infrastructure\ninvestment\n") +
  facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
  ylab("Type of tourists") +
  xlab("Infrastructure") +
  theme_ridges() +
  theme(strip.text.x = element_text(size = 12))


invest_services <- ggplot(data = aggregate(services ~ year + management + trends + tourists + iteration, 
                                 data = all_sims_socioecon, mean, na.rm=FALSE), 
                aes(x = year, y = services)) +
 geom_point(aes(colour = tourists), alpha = 0.7) +
 geom_smooth(aes(x = year, y = services, colour = tourists, group = tourists)) +
 scale_color_manual(values = pal, name = "Type of tourists") +
 scale_x_continuous(name = "Year", breaks = c(40, 43, 46, 49), position = "bottom") +
 scale_y_continuous(name = "Service investment", position = "left") +
 facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
 theme(text=element_text(size=12),                           
       panel.background = element_blank())

invest_services

ggplot(aggregate(services ~ year + management + trends + tourists + iteration, 
                                 data = all_sims_socioecon, mean, na.rm=FALSE), 
                aes(x = services, y = tourists)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
  scale_fill_viridis(name = "Service\ninvestment\n") +
  facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
  #xlim(0, 25000) +
  ylab("Type of tourists") +
  xlab("Service investment") +
  #scale_x_continuous(breaks = c(0, 10000, 20000), position = "bottom") +
  theme_ridges() +
  theme(strip.text.x = element_text(size = 12))


# visualising prices


price <- ggplot(data = aggregate(ticket_price ~ year + management + trends + tourists + iteration, 
                                 data = all_sims_socioecon, mean, na.rm=FALSE), 
                aes(x = year, y = ticket_price)) +
 geom_point(aes(colour = tourists), alpha = 0.7) +
 geom_smooth(aes(x = year, y = ticket_price, colour = tourists, group = tourists)) +
 scale_color_manual(values = pal, name = "Type of tourists") +
 scale_x_continuous(name = "Year", breaks = c(40, 43, 46, 49), position = "bottom") +
 scale_y_continuous(name = "Ticket price", position = "left") +
 facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
 theme(text=element_text(size=12),                           
       panel.background = element_blank())

price

ggplot(aggregate(ticket_price ~ year + management + trends + tourists + iteration, 
                                 data = all_sims_socioecon, mean, na.rm=FALSE), 
                aes(x = ticket_price, y = tourists)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
  scale_fill_viridis(name = "Ticket price\n") +
  facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
  ylab("Type of tourists") +
  xlab("Ticket price") +
  theme_ridges() +
  theme(strip.text.x = element_text(size = 12))


# profits

income <- ggplot(data = aggregate(money ~ year + management + trends + tourists + iteration, 
                                 data = all_sims_socioecon, mean, na.rm=FALSE), 
                aes(x = year, y = money)) +
 geom_point(aes(colour = tourists), alpha = 0.7) +
 geom_smooth(aes(x = year, y = money, colour = tourists, group = tourists)) +
 scale_color_manual(values = pal, name = "Type of tourists") +
 scale_x_continuous(name = "Year", breaks = c(40, 43, 46, 49), position = "bottom") +
 scale_y_continuous(name = "Income", position = "left") +
 facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
 theme(text=element_text(size=12),                           
       panel.background = element_blank())

income

ggplot(aggregate(money ~ year + management + trends + tourists + iteration, 
                                 data = all_sims_socioecon, mean, na.rm=FALSE), 
                aes(x = money, y = tourists)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
  scale_fill_viridis(name = "Income\n") +
  facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
  ylab("Type of tourists") +
  xlab("Income") +
  scale_x_continuous(breaks = c(0, 10000, 30000), position = "bottom") +
  theme_ridges() +
  theme(strip.text.x = element_text(size = 12))


# ratings

rating <- ggplot(data = aggregate(rating ~ year + management + trends + tourists + iteration, 
                                 data = all_sims_socioecon, mean, na.rm=FALSE), 
                aes(x = year, y = rating)) +
 geom_point(aes(colour = tourists), alpha = 0.7) +
 geom_smooth(aes(x = year, y = rating, colour = tourists, group = tourists)) +
 scale_color_manual(values = pal, name = "Type of tourists") +
 scale_x_continuous(name = "Year", breaks = c(40, 43, 46, 49), position = "bottom") +
 scale_y_continuous(name = "Rating", position = "left") +
 facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
 theme(text=element_text(size=12),                           
       panel.background = element_blank())

rating

ggplot(aggregate(rating ~ year + management + trends + tourists + iteration, 
                                 data = all_sims_socioecon, mean, na.rm=FALSE), 
                aes(x = rating, y = tourists)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
  scale_fill_viridis(name = "Rating\n") +
  facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
  ylab("Type of tourists") +
  xlab("Rating") +
  theme_ridges() +
  theme(strip.text.x = element_text(size = 12))



# time with animals

time_with <- ggplot(data = aggregate(time ~ year + management + trends + tourists + iteration, 
                                 data = all_sims_socioecon, mean, na.rm=FALSE), 
                aes(x = year, y = time)) +
 geom_point(aes(colour = tourists), alpha = 0.7) +
 geom_smooth(aes(x = year, y = time, colour = tourists, group = tourists)) +
 scale_color_manual(values = pal, name = "Type of tourists") +
 scale_x_continuous(name = "Year", breaks = c(40, 43, 46, 49), position = "bottom") +
 scale_y_continuous(name = "Time with animals", position = "left") +
 facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
 theme(text=element_text(size=12),                           
       panel.background = element_blank())

time_with

ggplot(aggregate(time ~ year + management + trends + tourists + iteration, 
                                 data = all_sims_socioecon, mean, na.rm=FALSE), 
                aes(x = time, y = tourists)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
  scale_fill_viridis(name = "Time with\nanimals\n") +
  facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
  ylab("Type of tourists") +
  xlab("Time with animals") +
  theme_ridges() +
  theme(strip.text.x = element_text(size = 12))


# defection

defectors <- ggplot(data = aggregate(defect ~ year + management + trends + tourists + iteration, 
                                 data = subset(all_sims_socioecon, management!="Code of conduct"), 
                                 mean, na.rm=FALSE) , 
                    aes(x = year, y = defect)) +
 geom_point(aes(colour = tourists), alpha = 0.7) +
 geom_smooth(aes(x = year, y = defect, colour = tourists, group = tourists)) +
 scale_color_manual(values = pal, name = "Type of tourists") +
 scale_x_continuous(name = "Year", breaks = c(40, 43, 46, 49), position = "bottom") +
 scale_y_continuous(name = "Times defecting", position = "left") +
 facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
 theme(text=element_text(size=12),                           
       panel.background = element_blank())

defectors

ggplot(aggregate(defect ~ year + management + trends + tourists + iteration, 
                                 data = subset(all_sims_socioecon, management!="Code of conduct"), 
                                 mean, na.rm=FALSE) , 
                    aes(x = defect, y = tourists)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
  scale_fill_viridis(name = "Times defecting\n") +
  facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
  ylab("Type of tourists") +
  xlab("Times defecting") +
  theme_ridges() +
  theme(strip.text.x = element_text(size = 12))



# effect on wildlife

effect <- ggplot(data = all_sims_ecol, aes(x = year, y = effect)) +
 geom_point(aes(colour = tourists), alpha = 0.7) +
 geom_smooth(aes(x = year, y = effect, colour = tourists, group = tourists)) +
 scale_color_manual(values = pal, name = "Type of tourists") +
 scale_x_continuous(name = "Year", breaks = c(40, 43, 46, 49), position = "bottom") +
 scale_y_continuous(name = "Effect on wildlife", position = "left") +
 facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
 theme(text=element_text(size=12),                           
       panel.background = element_blank())
effect

encounter <- ggplot(data = all_sims_ecol, aes(x = year, y = p_encounter)) +
 geom_point(aes(colour = tourists), alpha = 0.7) +
 geom_smooth(aes(x = year, y = p_encounter, colour = tourists, group = tourists)) +
 scale_color_manual(values = pal, name = "Type of tourists") +
 scale_x_continuous(name = "Year", breaks = c(40, 43, 46, 49), position = "bottom") +
 scale_y_continuous(name = "Probability of encounter", position = "left") +
 facet_wrap(~ management + trends, ncol = 3, nrow = 6) +
 theme(text=element_text(size=12),                           
       panel.background = element_blank())

encounter

# maximum_time <- ggplot(data = all_sims_ecol, aes(x = year, y = time_max)) +
#  geom_point(aes(colour = tourists), alpha = 0.7) +
#  geom_smooth(aes(x = year, y = time_max, colour = tourists, group = tourists)) +
#  scale_color_manual(values = pal, name = "Type of tourists") +
#  scale_x_continuous(name = "Year", breaks = c(40, 43, 46, 49), position = "bottom") +
#  scale_y_continuous(name = "Maximum time", position = "left") +
#  facet_wrap(~ trends + management) +
#  theme(text=element_text(size=12),                           
#        panel.background = element_blank())
# 
# maximum_time
# 

# how many tour operators are still in business at the end of the 50 years?

n_tourops <- setDT(all_sims_socioecon)[, .(count = uniqueN(id)), by = c("trends", "tourists", "management", "iteration", "year")]


ggplot(data = n_tourops) +
  geom_jitter(aes(x = tourists, y = count), shape = 20, color = "black", size = 1) +
  geom_boxplot(aes(x = tourists, y = count, fill = tourists), outlier.shape = NA, alpha = 0.8) +
  scale_fill_manual(values = pal, name = "Type of tourists") +
  ylab("N operators in business") +
  xlab("Type of tourists") +
  facet_wrap(~ management + trends, ncol = 3, nrow = 6)  +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        text=element_text(size=12), panel.grid.major = element_blank(),                           
        panel.background = element_blank())


####### sustainability ######

# we look at overall sustainability of the tourism SES
# by looking at which scenario can achieve high income,
# keep tour operators in business and healthy wildlife

# we calculate a mean of the tour operators income per year and per simulation

all_sims_income_agg <- aggregate(money ~ management + trends + tourists + iteration + year, 
                                 all_sims_socioecon, mean, na.rm=FALSE)
# then we put all the data together

all_sims_agg <- merge(all_sims_income_agg, n_tourops, 
                      by = c("iteration", "management", "tourists", "trends" , "year"))

all_sims_agg <- merge(all_sims_agg, all_sims_ecol, 
                      by = c("iteration", "management", "tourists", "trends" , "year"))

all_sims_mean <- aggregate(. ~ management + trends + tourists + iteration, all_sims_agg, mean, na.rm=FALSE)

new_pal <- viridis(3, begin = 0, end = 0.9)


bubble <- ggplot(data = all_sims_mean) +
          geom_point(aes(x = money, y = count, color = tourists, size = effect), 
                     shape = 16, alpha = 0.8) +
          scale_size(range = c(4,13), name = "Effect on\nwildlife\n") +
          scale_color_manual(values = new_pal, name = "Type of\ntourists\n") +
          ylim(0,25) +
          #xlim(-1000,17000) +
          xlab("Income") +
          ylab("N operators") +
          facet_wrap(~ management + trends, ncol = 3, nrow = 6)+
          theme(text=element_text(size=12),
                panel.grid.major = element_blank(),                           
                panel.background = element_blank()) +
          guides(color = guide_legend(override.aes = list(size=8)))

bubble
