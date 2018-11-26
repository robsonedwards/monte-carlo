# 
require("dslabs") 
require("dplyr")
require("ggplot2")
require("magrittr")
set.seed(4113)

################################ Data Wrangling ################################

data("polls_us_election_2016")

polls_us_election_2016 %>% 
  # Ignore data from individual congressional districts of Maine and Nebraska
  filter(!grepl("Maine CD-", state) & !grepl("Nebraska CD-", state) ) %>%
  mutate(clinton_margin = rawpoll_clinton - rawpoll_trump) %>%
  mutate(duration = enddate - startdate) %>%
  # Remove unneeded columns 
  select(-adjpoll_clinton, -adjpoll_trump, -adjpoll_johnson, -adjpoll_mcmullin,
         -rawpoll_clinton, -rawpoll_trump, -rawpoll_johnson, -rawpoll_mcmullin,
         -startdate, -population) -> #-pollster#??
  polls
polls$state <- as.factor(as.character(polls$state))

results_us_election_2016 %>%
  select(-electoral_votes) -> 
  results

# Create a result row for U.S. as a whole rather than an individual state
U.S. <- c(clinton = 65844610, trump = 62979636, others = 7804213) #Source: Time
U.S. <- round( U.S. / sum(U.S.) * 100, digits = 1)
results[52,] <- c("U.S.", U.S.["clinton"], U.S.["trump"], U.S.["others"])

# Change datatype of cols in results to agree with cols in polls
results$state <- as.factor(results$state)
results$clinton <- as.numeric(results$clinton)
results$trump <- as.numeric(results$trump)
results$others <- as.numeric(results$others)

# Create error column. Error is the amount the poll margin exceeded the result
# margin in the direction of favouring Clinton. (see explanation in report)
results %<>% mutate(clinton_margin = clinton - trump)
for(i in 1:nrow(polls)){
  polls$result[i] = results$clinton_margin[results$state == polls$state[i]]
}
polls %>% 
  mutate(error = clinton_margin - result) %>%
  mutate(error2 = error ^2) %>%
  select(-result, -clinton_margin) -> #remove unneeded columns
  polls

polls$grade <- addNA(polls$grade) 
  #So we can see how polls 538 didn't grade performed

# Remove 43 outliers that are > 3 sd's from the mean. (top 1% worst polls)
polls %<>% filter(abs(polls$error - mean(polls$error)) < 3 * sd(polls$error) )

# Exploratory Data Analysis
polls_sample = polls[sample(1:nrow(polls), size = 150, replace = F),]
plot(polls_sample$enddate, polls_sample$error2, pch = '+')
lm(polls$error2 ~ polls$enddate)

goodpolls <- filter(polls, grepl("A", grade))
plot(goodpolls$enddate, goodpolls$error2, pch = '+')
lm(goodpolls$error2 ~ goodpolls$enddate)

# No evidence of correlation between date and poll error in general
cor.test(polls$error, as.numeric(polls$enddate))
# Some evidence of correlation for higher-rated polls
cor.test(goodpolls$error, as.numeric(goodpolls$enddate))

################################## Simulation ##################################



############################## Hypothesis Testing ##############################



################################################################################

# Garbage collection!! 
rm(i, U.S., goodpolls, polls_sample, polls_us_election_2016, results_us_election_2016)

