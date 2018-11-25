# 
require("dslabs") 
require("dplyr")
require("ggplot2")

################################ Data Wrangling ################################

data("polls_us_election_2016")

polls_us_election_2016 %>% 
  # Ignore data from individual congressional districts of Maine and Nebraska
  filter(!grepl("Maine CD-", state) & !grepl("Nebraska CD-", state) ) %>%
  mutate(clinton_margin = rawpoll_clinton - rawpoll_trump) %>%
  mutate(duration = enddate - startdate) %>%
  select(-adjpoll_clinton, -adjpoll_trump, -adjpoll_johnson, -adjpoll_mcmullin,
         -rawpoll_clinton, -rawpoll_trump, -rawpoll_johnson, -rawpoll_mcmullin,
         -startdate) ->
  polls
results_us_election_2016 %>%
  select(-electoral_votes) -> 
  results

# Create a result for U.S. as a whole
U.S. <- c(clinton = 65844610, trump = 62979636, others = 7804213) #Source: Time
U.S. <- round( U.S. / sum(U.S.) * 100, digits = 1)
results[52,] <- c("U.S.", U.S.["clinton"], U.S.["trump"], U.S.["others"])
# Convert results$state to factor to agree with polls$state
results$state <- as.factor(results$state)

# Exploratory Data Analysis
polls_sample = polls[sample(1:nrow(polls), size = 50, replace = F),]
plot(polls_sample$enddate, polls_sample$clinton_margin)
lm(polls_sample$clinton_margin ~ polls_sample$enddate)

################################## Simulation ##################################

############################## Hypothesis Testing ##############################

