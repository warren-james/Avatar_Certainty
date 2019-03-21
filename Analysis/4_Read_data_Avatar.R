#### Avatar script ####
# read in the data files and save them 
# for Essex data

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
results_files <- dir("data/Aberdeen/Screen_info/")

# setup data_frame 
df_Aberdeen_screen_info <- data.frame(participant = character(),
                                      x_res = numeric(),
                                      y_res = numeric())

df_Aberdeen_deltas <- data.frame(participant = character(),
                                 delta = numeric())

df_Aberdeen_beta <- data.frame(participant = character(),
                               beta1 = numeric(),
                               beta2 = numeric())

df_Aberdeen_avatar_info <- data.frame(participant = character(),
                                      max_speed = numeric(),
                                      reach = numeric())

# loop throuh and read in data 
for(f in unique(results_files)){
  # read in file
  temp_df <- readMat(paste("data/Aberdeen/Screen_info/", f, sep = ""))
  
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
  df_Aberdeen_screen_info <- rbind(df_Aberdeen_screen_info, data.frame(participant = Participant,
                                                                       x_res = x_res,
                                                                       y_res = y_res))
  # deltas 
  df_Aberdeen_deltas <- rbind(df_Aberdeen_deltas, data.frame(participant = Participant, 
                                                             delta = Delta))
  # beta
  df_Aberdeen_beta <- rbind(df_Aberdeen_beta, data.frame(participant = Participant,
                                                         beta1 = beta1,
                                                         beta2 = beta2))
  
  # avatar 
  df_Aberdeen_avatar_info <- rbind(df_Aberdeen_avatar_info, data.frame(participant = Participant,
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
save(df_Aberdeen_beta, file = "scratch/data/df_Aberdeen_beta")
save(df_Aberdeen_deltas, file = "scratch/data/df_Aberdeen_deltas")
save(df_Aberdeen_screen_info, file = "scratch/data/df_Aberdeen_screen_info")
save(df_Aberdeen_avatar_info, file = "scratch/data/df_Aberdeen_avater_info")


#### Read in Decision data ####
# setpath 
results_files <- dir("data/Aberdeen/Decisions/")

# setup data.frame
df_Aberdeen_decisions <- data.frame(participant = character(),
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
  d <- read.csv(paste("data/Aberdeen/Decisions/", f, sep = ""), header = T)
  
  # Participant 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[1]
  
  # add in Participant
  d$participant <- Participant
  
  # combine data
  df_Aberdeen_decisions <- rbind(df_Aberdeen_decisions, d)
}

# tidy 
rm(d, f, Participant, results_files)

# arrange this 
df_Aberdeen_decisions <- select(df_Aberdeen_decisions,
                                participant,
                                everything())

# sort colnames to be lower case 
names(df_Aberdeen_decisions) <- tolower(names(df_Aberdeen_decisions))

# add in an order variable 
# empty vector
rand_first <- c()

# loop through to classify
for(p in unique(df_Aberdeen_decisions$participant)){
  # get subset
  ss <- df_Aberdeen_decisions[df_Aberdeen_decisions$participant == p,] 
  
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
df_Aberdeen_decisions$rand_first <- rand_first

# tidy
rm(first_cond, p, rand_first, rf, ss)

# labels for spread 
df_Aberdeen_decisions$truck_perf <- "Variable"
df_Aberdeen_decisions$truck_perf[df_Aberdeen_decisions$spread > 1] = "Constant"

# add in a standard sep 
for(p in unique(df_Aberdeen_decisions$participant)){
  df_Aberdeen_decisions$standard[df_Aberdeen_decisions$participant == p] <- as.numeric(as.factor(df_Aberdeen_decisions$Delta[df_Aberdeen_decisions$participant == p]))
}

rm(p)

# add in the max_speed info 
df_Aberdeen_decisions <- merge(df_Aberdeen_decisions, df_Aberdeen_avatar_info)

# add in chance of success 
df_Aberdeen_decisions <- df_Aberdeen_decisions %>% 
  rowwise() %>%
  mutate(chance = prob_success(placed_x, delta, spread, max_speed)) %>%
  ungroup()

# add in condition labels 
df_Aberdeen_decisions$condition_label <- "abstract"
df_Aberdeen_decisions$condition_label[df_Aberdeen_decisions$condition == 2] <- "truck"


# save this
save(df_Aberdeen_decisions, file = "scratch/data/df_Aberdeen_decisions")

# # make quick plot 
# df_Aberdeen_decisions %>%
#   mutate(norm_dist = abs(Placed_x)/Delta,
#          Skew = as.factor(Skew)) %>%
#   ggplot(aes(Delta, norm_dist, colour = Skew)) +
#   geom_point() +
#   facet_wrap(~ Participant + Spread, ncol = 2)


#### Read in Estimates ####
# setup estimates path
results_files <- dir("data/Aberdeen/Estimates/")

# setup dataframe
df_Aberdeen_estimates <- c(participant = character(),
                           delta = numeric(),
                           estimate = numeric())

# loop to read 
for(f in results_files){
  # read file
  d <- read.csv(paste("data/Aberdeen/Estimates/", f, sep = ""), header = T)
  
  # Participant 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[1]
  
  # add in Participant
  d$participant <- Participant
  
  # bind to data frame
  df_Aberdeen_estimates <- rbind(df_Aberdeen_estimates, d)
}

# tidy
rm(d, f, Participant, results_files)

# arrange 
df_Aberdeen_estimates <- select(df_Aberdeen_estimates,
                                participant,
                                everything())

# add in a label to use later 
df_Aberdeen_estimates$estimate_type <- "Participant"

# add in truck_perf
df_Aberdeen_estimates$truck_perf <- "Variable"
df_Aberdeen_estimates$truck_perf[df_Aberdeen_estimates$Spread > 1] = "Constant"

# sort to lower case
names(df_Aberdeen_estimates) <- tolower(names(df_Aberdeen_estimates))

# save this file 
save(df_Aberdeen_estimates, file = "scratch/data/df_Aberdeen_estimates")


#### Read in click history ####
# set path 
results_files <- dir("data/Aberdeen/Click_history/")

# empty frame
df_Aberdeen_clickhist <- data.frame(participant = character(),
                                    block = numeric(),
                                    trial = numeric(),
                                    time_taken = numeric(),
                                    delta = numeric(),
                                    num_clicks = numeric(),
                                    placed_x = numeric())

# loop to read in data 
for(f in results_files){
  # read file
  d <- read.csv(paste("data/Aberdeen/Click_history/", f, sep = ""), header = T)
  
  # Participant 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[1]
  
  # add in Participant
  d$participant <- Participant
  
  # bind 
  df_Aberdeen_clickhist <- rbind(df_Aberdeen_clickhist, d)
}

# tidy 
rm(d, f, Participant, results_files)

# arrange 
df_Aberdeen_clickhist <- select(df_Aberdeen_clickhist,
                                participant, 
                                everything())

# sort out names 
names(df_Aberdeen_clickhist) <- tolower(names(df_Aberdeen_clickhist))

# save this
save(df_Aberdeen_clickhist, file = "scratch/data/df_Aberdeen_clickhist")

#### Read in Demo_phase ####
results_files <- dir("data/Aberdeen/Demo_phase/")

# empty frame
df_Aberdeen_demo_phase <- data.frame(participant = character(),
                                     trial = numeric(),
                                     success = numeric())

for(f in results_files){
  # make file
  d <- read.csv(paste("data/Aberdeen/Demo_phase/", f, sep = ""), header = T)
  
  # Participant 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[1]
  
  # add in Participant
  d$participant <- Participant
  
  # bind together
  df_Aberdeen_demo_phase <- rbind(df_Aberdeen_demo_phase, d)
}

# tidy 
rm(d, f, Participant, results_files)

# reorder 
df_Aberdeen_demo_phase <- select(df_Aberdeen_demo_phase,
                                 participant,
                                 everything())

# add in type 
df_Aberdeen_demo_phase$Estimate_Type <- "Demo"

# add in truck_perf
df_Aberdeen_demo_phase$truck_perf <- "Variable"
df_Aberdeen_demo_phase$truck_perf[df_Aberdeen_demo_phase$Spread > 1] = "Constant"

# lower case 
names(df_Aberdeen_demo_phase) <- tolower(names(df_Aberdeen_demo_phase))

# save this 
save(df_Aberdeen_demo_phase, file = "scratch/data/df_Aberdeen_demo_phase")

#### Read in Confidence ####
# set path
results_files <- dir("data/Aberdeen/Confidence/")

# setup data frame 
df_Aberdeen_confidence <- data.frame(Participant = character(),
                                     Delta = numeric(),
                                     Y_or_N = numeric(),
                                     Confidence = numeric(),
                                     RT = numeric())

# loop through and save 
for(f in results_files){
  # read in data 
  d <- read.csv(paste("data/Aberdeen/Confidence/", f, sep = ""))
  
  # Get Participant number 
  Participant <- strsplit(f, '[_]')[[1]]
  Participant <- Participant[1]
  
  # add in Particiapnt number 
  d$Participant <- Participant
  
  # combine datasets 
  df_Aberdeen_confidence <- rbind(df_Aberdeen_confidence, d)
  
}

# tidy 
rm(f, Participant, results_files, d)

# lower case 
names(df_Aberdeen_confidence) <- tolower(names(df_Aberdeen_confidence))

# save 
save(df_Aberdeen_confidence, file = "scratch/data/df_Aberdeen_confidence")



#### Read in Demographics ####
# set path
results_files <- dir("data/Aberdeen/Demographics/")

# setup data_frame 
df_Aberdeen_demographics <- data.frame(participant = character(),
                                       age = numeric(),
                                       gender = numeric())

# loop throuh and read in data 
for(f in unique(results_files)){
  # read in file
  temp_df <- readMat(paste("data/Aberdeen/Demographics/", f, sep = ""))
  
  # get participant name 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[1]
  
  # age
  age <- as.numeric(temp_df$age[1])
  
  # gender 
  gender <- temp_df$gender[1]
  # fill data frame 
  # demographics
  df_Aberdeen_demographics <- rbind(df_Aberdeen_demographics, data.frame(participant = Participant,
                                                                         age = age,
                                                                         gender = gender))
}

# need to sort out levels 
df_Aberdeen_demographics$gender <- tolower(df_Aberdeen_demographics$gender)
df_Aberdeen_demographics$gender[df_Aberdeen_demographics$gender == "f"] <- "female"
# save this 
save(df_Aberdeen_demographics, file = "scratch/data/df_Aberdeen_demographics")

# clear environment 
rm(list = ls())




