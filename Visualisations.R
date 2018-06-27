# Visualisations ####
# Author: Francesca Mancini
# Date created: 2018-06-27
# Date modified: 

# tansform list into a dataframe

bookings_year_df <- do.call("rbind", bookings_year)

# visualise bookings
# create a grouping variable for visualisation

# bookings_year_df <- bookings_year_df %>%
#  mutate(groups = case_when(id <= 10 ~ 1,
#                            id > 10 & id <= 20 ~ 2,
#                            id > 20 & id <= 30 ~ 3,
#                            id > 30 & id <= 40 ~ 4,
#                            id > 40 & id <= 50 ~ 5,
#                            id > 50 & id <= 60 ~ 6,
#                            id > 60 & id <= 70 ~ 7))
# 

# visualise time series of number of bookings per tour operator

books <- ggplot(data = bookings_year_df, aes(x = year, y = bookings, colour = as.factor(id))) +
 geom_point() +
 geom_line(aes(group = as.factor(id))) +
 facet_wrap(~id)

# visualising investments

investments_df <- do.call("rbind", investments)

invest <- ggplot(data = investments_df, aes(x = year, y = infrastructure, colour = as.factor(id))) +
 geom_point() +
 geom_line(aes(group = as.factor(id))) +
 facet_wrap(~id)

# they are not investing any money into infrastructure, maybe I need to make it easier

invest_services <- ggplot(data = investments_df, aes(x = year, y = services, colour = as.factor(id))) +
 geom_point() +
 geom_line(aes(group = as.factor(id))) +
 facet_wrap(~id)

# same here

# visualising prices

price_df <- do.call("rbind", prices)

price <- ggplot(data = price_df, aes(x = year, y = ticket_price, colour = as.factor(id))) +
 geom_point() +
 geom_line(aes(group = as.factor(id))) +
 facet_wrap(~id)

# the prices are going down too quickly and too much

# profits

profits_df <- do.call("rbind", profits)

income <- ggplot(data = profits_df, aes(x = year, y = money, colour = as.factor(id))) +
 geom_point() +
 geom_line(aes(group = as.factor(id))) +
 facet_wrap(~id)

# because prices go down so much so do the profits

# ratings

ratings_df <- do.call("rbind", ratings_year)

rating <- ggplot(data = ratings_df, aes(x = year, y = rating, colour = as.factor(id))) +
 geom_point() +
 geom_line(aes(group = as.factor(id))) +
 facet_wrap(~id)

# rating is not changing much probably because investments are almost 0

# time with animals

withanimals_df <- do.call("rbind", withanimals)

time_with <- ggplot(data = withanimals_df, aes(x = year, y = time, colour = as.factor(id))) +
 geom_point() +
 geom_line(aes(group = as.factor(id))) +
 facet_wrap(~id)

# this is what is expected

# effect on wildlife
par(mfrow = c(2,2))
plot(effects)
plot(encounter_probs)
plot(max_times)
# this is what is expected

