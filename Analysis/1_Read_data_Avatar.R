#### Avatar script ####
# read in the data files and save them 

#### Library ####
library(tidyverse)
library(R.matlab)

#### functions ####
# get probability of success
prob_success <- function(pos, delta, beta, max_speed){
  # get observations
  y <- (round(rbeta(100000, beta, beta)*max_speed)+1) * travel_time + 30
  
  # get prob of success 
  chance1 <- sum(y >= delta - abs(pos))/length(y)
  chance2 <- sum(y >= delta + abs(pos))/length(y)
  
  # output 
  chance <- (chance1 + chance2)/2 
  return(chance)
}

#### Constants ####
travel_time <- 100

#### Read in screen data ####
# set path
results_files <- dir("data/Essex/Screen_info/")

# setup data_frame 
df_screen_info <- data.frame(participant = character(),
                             x_res = numeric(),
                             y_res = numeric())

df_deltas <- data.frame(participant = character(),
                        delta = numeric())

df_beta <- data.frame(participant = character(),
                      beta1 = numeric(),
                      beta2 = numeric())

df_avatar_info <- data.frame(participant = character(),
                             max_speed = numeric(),
                             reach = numeric())

# loop throuh and read in data 
for(f in unique(results_files)){
  # read in file
  temp_df <- readMat(paste("data/Essex/Screen_info/", f, sep = ""))
  
  # get participant name 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[1]
  
  # resolutions
  x_res <- as.numeric(temp_df$params[1])*2
  y_res <- as.numeric(temp_df$params[2])*2
  
  # betas
  betas <- temp_df$box[[2]]
  beta1 <- betas[1]
  beta2 <- betas[2]
  
  # Delta 
  Delta <- unlist(temp_df$params[3])
  
  # reach 
  reach <- as.numeric(temp_df$box[4])
  
  # max_speed 
  max_speed <- as.numeric(temp_df$box[1])
  
  # fill data frame 
  # screen
  df_screen_info <- rbind(df_screen_info, data.frame(participant = Participant,
                                                     x_res = x_res,
                                                     y_res = y_res))
  # deltas 
  df_deltas <- rbind(df_deltas, data.frame(participant = Participant, 
                                           delta = Delta))
  # beta
  df_beta <- rbind(df_beta, data.frame(participant = Participant,
                                       beta1 = beta1,
                                       beta2 = beta2))
  
  # avatar 
  df_avatar_info <- rbind(df_avatar_info, data.frame(participant = Participant,
                                                     max_speed = max_speed,
                                                     reach = reach))
}

# tidy 
rm(betas,
   temp_df,
   beta1,
   beta2,
   Delta,
   f,
   max_speed,
   Participant,
   reach,
   results_files,
   x_res,
   y_res)

# save these files 
save(df_beta, file = "scratch/data/df_beta")
save(df_deltas, file = "scratch/data/df_deltas")
save(df_screen_info, file = "scratch/data/df_screen_info")
save(df_avatar_info, file = "scratch/data/df_avater_info")


#### Read in Decision data ####
# setpath 
results_files <- dir("data/Essex/Decisions/")

# setup data.frame
df_decisions <- data.frame(participant = character(),
                           condition = character(),
                           spread = character(),
                           block = numeric(),
                           trial = numeric(),
                           delta = numeric(),
                           RT = numeric(),
                           initial_X = numeric(),
                           placed_X = numeric(),
                           target_side = numeric(),
                           speed = numeric(),
                           success = numeric())

# loop through files 
for(f in results_files){
  # read in file
  d <- read.csv(paste("data/Essex/Decisions/", f, sep = ""), header = T)
  
  # Participant 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[1]
  
  # add in Participant
  d$participant <- Participant
  
  # combine data
  df_decisions <- rbind(df_decisions, d)
}

# tidy 
rm(d, f, Participant, results_files)

# arrange this 
df_decisions <- select(df_decisions,
                       participant,
                       everything())

# sort colnames to be lower case 
names(df_decisions) <- tolower(names(df_decisions))

# add in an order variable 
# empty vector
rand_first <- c()

# loop through to classify
for(p in unique(df_decisions$participant)){
  # get subset
  ss <- df_decisions[df_decisions$participant == p,] 
  
  # get first block condition
  first_cond <- ss$spread[ss$block == 1 & ss$trial == 1]
  # check block 
  if(first_cond == 1){
    rf <- 1
  } else {
    rf <- 0
  }
  
  # add to empty vector
  rand_first <- c(rand_first, rep(rf, nrow(ss)))
}

# add to data frame
df_decisions$rand_first <- rand_first

# tidy
rm(first_cond, p, rand_first, rf, ss)

# labels for spread 
df_decisions$truck_perf <- "Variable"
df_decisions$truck_perf[df_decisions$spread > 1] = "Constant"

# add in a standard sep 
for(p in unique(df_decisions$participant)){
  df_decisions$standard[df_decisions$participant == p] <- as.numeric(as.factor(df_decisions$Delta[df_decisions$participant == p]))
}

rm(p)

# add in the max_speed info 
df_decisions <- merge(df_decisions, df_avatar_info)

# add in chance of success 
df_decisions <- df_decisions %>% 
  rowwise() %>%
  mutate(chance = prob_success(placed_x, delta, spread, max_speed)) %>%
  ungroup()


# save this
save(df_decisions, file = "scratch/data/df_decisions")

# # make quick plot 
# df_decisions %>%
#   mutate(norm_dist = abs(Placed_x)/Delta,
#          Skew = as.factor(Skew)) %>%
#   ggplot(aes(Delta, norm_dist, colour = Skew)) +
#   geom_point() +
#   facet_wrap(~ Participant + Spread, ncol = 2)


#### Read in Estimates ####
# setup estimates path
results_files <- dir("data/Essex/Estimates/")

# setup dataframe
df_estimates <- c(participant = character(),
                  delta = numeric(),
                  estimate = numeric())

# loop to read 
for(f in results_files){
  # read file
  d <- read.csv(paste("data/Essex/Estimates/", f, sep = ""), header = T)
  
  # Participant 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[1]
  
  # add in Participant
  d$participant <- Participant
  
  # bind to data frame
  df_estimates <- rbind(df_estimates, d)
}

# tidy
rm(d, f, Participant, results_files)

# arrange 
df_estimates <- select(df_estimates,
                       participant,
                       everything())

# add in a label to use later 
df_estimates$estimate_type <- "Participant"

# add in truck_perf
df_estimates$truck_perf <- "Variable"
df_estimates$truck_perf[df_estimates$Spread > 1] = "Constant"

# sort to lower case
names(df_estimates) <- tolower(names(df_estimates))

# save this file 
save(df_estimates, file = "scratch/data/df_estimates")


#### Read in click history ####
# set path 
results_files <- dir("data/Essex/Click_history/")

# empty frame
df_clickhist <- data.frame(participant = character(),
                           block = numeric(),
                           trial = numeric(),
                           time_taken = numeric(),
                           delta = numeric(),
                           num_clicks = numeric(),
                           placed_x = numeric())

# loop to read in data 
for(f in results_files){
  # read file
  d <- read.csv(paste("data/Essex/Click_history/", f, sep = ""), header = T)
  
  # Participant 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[1]
  
  # add in Participant
  d$participant <- Participant
  
  # bind 
  df_clickhist <- rbind(df_clickhist, d)
}

# tidy 
rm(d, f, Participant, results_files)

# arrange 
df_clickhist <- select(df_clickhist,
                       participant, 
                       everything())

# sort out names 
names(df_clickhist) <- tolower(names(df_clickhist))

# save this
save(df_clickhist, file = "scratch/data/df_clickhist")

#### Read in Demo_phase ####
results_files <- dir("data/Essex/Demo_phase/")

# empty frame
df_demo_phase <- data.frame(participant = character(),
                            trial = numeric(),
                            success = numeric())

for(f in results_files){
  # make file
  d <- read.csv(paste("data/Essex/Demo_phase/", f, sep = ""), header = T)
  
  # Participant 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[1]
  
  # add in Participant
  d$participant <- Participant
  
  # bind together
  df_demo_phase <- rbind(df_demo_phase, d)
}

# tidy 
rm(d, f, Participant, results_files)

# reorder 
df_demo_phase <- select(df_demo_phase,
                        Participant,
                        everything())

# add in type 
df_demo_phase$Estimate_Type <- "Demo"

# add in truck_perf
df_demo_phase$truck_perf <- "Variable"
df_demo_phase$truck_perf[df_demo_phase$Spread > 1] = "Constant"

# lower case 
names(df_demo_phase) <- tolower(names(df_demo_phase))

# save this 
save(df_demo_phase, file = "scratch/data/df_demo_phase")

#### Read in Confidence ####
# set path
results_files <- dir("data/Essex/Confidence/")

# setup data frame 
df_confidence <- data.frame(Participant = character(),
                            Delta = numeric(),
                            Y_or_N = numeric(),
                            Confidence = numeric(),
                            RT = numeric())

# loop through and save 
for(f in results_files){
  # read in data 
  d <- read.csv(paste("data/Essex/Confidence/", f, sep = ""))
  
  # Get Participant number 
  Participant <- strsplit(f, '[_]')[[1]]
  Participant <- Participant[1]
  
  # add in Particiapnt number 
  d$Participant <- Participant
  
  # combine datasets 
  df_confidence <- rbind(df_confidence, d)
  
}

# tidy 
rm(f, Participant, results_files, d)

# lower case 
names(df_confidence) <- tolower(names(df_confidence))

# save 
save(df_confidence, file = "scratch/data/df_confidence")

# clear environment 
rm(list = ls())




