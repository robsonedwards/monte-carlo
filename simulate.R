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

#polls$grade <- addNA(polls$grade) 
  #So we can see how polls by pollsters 538 didn't grade performed

polls$enddate <- as.numeric(polls$enddate) - as.numeric(as.Date("2016-11-08"))
  # Now enddate is the number of days before the election, this makes intercept
  # terms in the linear models we're about to derive much more interpretable

# Remove 43 outliers that are > 3 sd's from the mean. (top 1% worst polls)
polls %<>% filter(abs(polls$error - mean(polls$error)) < 3 * sd(polls$error) )

########################## Exploratory Data Analysis ###########################
polls_sample = polls[sample(1:nrow(polls), size = 150, replace = F),]
plot(polls_sample$enddate, polls_sample$error2, pch = '+')
lm(polls$error2 ~ polls$enddate)
lm(polls$error ~ polls$enddate)

goodpolls <- filter(polls, grepl("A", grade))
plot(goodpolls$enddate, goodpolls$error2, pch = '+')
lm(goodpolls$error2 ~ goodpolls$enddate)
lm(goodpolls$error ~ goodpolls$enddate)

# No evidence of correlation between date and poll error in general
cor.test(polls$error, polls$enddate)
cor.test(abs(polls$error), polls$enddate)
cor.test(polls$error2, polls$enddate)

# Some evidence of correlation for higher-rated polls, for some error measures
cor.test(goodpolls$error, goodpolls$enddate)
cor.test(abs(goodpolls$error), goodpolls$enddate)
cor.test(goodpolls$error2, goodpolls$enddate)

################################## Simulation ##################################

simulate_and_test <- function(data_subset, n, effect_size, test){
  # NOTE test must take only x and y and return p value 
  # simulate
  mean_error = mean(data_subset$error)
  mean_enddate = mean(data_subset$enddate)
  sd_error = sd(data_subset$error)
  sim <- data.frame(enddate = sample(as.integer(data_subset$enddate), n * 100, replace = TRUE))
  sim$error <- rnorm(n * 100, sd = sd_error, mean =  (sim$enddate - mean_enddate) * effect_size + mean_error)
  # test
  pvals <- 1:100
  apply_test <- function(i){
    range <- (i * 100 - 99):(i * 100)
    return(test(sim$error[range], sim$enddate[range]))
  }
  pvals <- lapply(pvals, apply_test)
  pvals <- unlist(pvals)
  result <- sum(pvals < 0.05) # size or power. 
  return(result / 100)
}

pearson <- function(x, y){
  test <- cor.test(x, y, alternative = "less", method = "pearson")
  return(unname(test["p.value"]) )
}

spearman <- function(x, y){
  test <- cor.test(x, y, alternative = "less", method = "spearman")
  return(unname(test["p.value"]))
}

for(method in c(pearson, spearman)){
  for(effect_size in c(0, -0.001, -0.01, -0.05)){
    for(g in levels(polls$grade)){
      data <- polls[polls$grade == g,]
      for(n in c(100, 1000, 4000)){
        print(as.character( c("Effect:", effect_size,  
                ". Grade:", g, ". n: ", n, ". Result: ", 
                simulate_and_test(data, n, effect_size, method))))
      }
    }
  }
}

# get_table <- function(effect_size, method){
#   get_cell <- function(i, n, effect_size, method){
#     data_subset <- data_subsets[i]
#     return (simulate_and_test(data_subset, n, effect_size, method))
#   }
#   n100 = lapply(1:12, function(i){get_cell(i, 100, effect_size, method)})
#   n1000 = lapply(1:12, function(i){get_cell(i, 1000, effect_size, method)})
#   n4000 = lapply(1:12, function(i){get_cell(i, 4000, effect_size, method)})
#   
#   table <- data.frame(n100, n1000, n4000)
#   rownames(table)<- c(levels(polls$grade), "all")
#   return(table)
# }
# 
# make_hypothesis_tables <- function(){
#   for(effect_size in c(0, -0.001, -0.01, -0.05)){
#     for(method in c(pearson, spearman)){
#       get_table(effect_size, method)
#     }
#   }
# }
# 
# get_data_subsets <- function(){
#   data_subsets <- list()
#   for(i in 1:length(levels(polls$grade))){
#     data_subsets <- c(data_subsets, 
#                       list(polls[polls$grade == levels(polls$grade)[i],]))
#   }
#   data_subsets <- c(data_subsets, list(polls))
#   return(data_subsets)
# }
# 
# data_subsets <- get_data_subsets()
# 
# make_hypothesis_tables()

############################# Garbage collection!! #############################
rm(i, U.S., goodpolls, polls_sample, polls_us_election_2016, results_us_election_2016)

