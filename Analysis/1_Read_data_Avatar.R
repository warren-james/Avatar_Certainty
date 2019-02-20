#### Avatar script ####
# read in the data files and save them 

#### Library ####
library(tidyverse)
library(R.matlab)

#### functions ####
# do we need any? 

#### Constants ####
# again, probably don't need 

#### Read in screen data ####
# set path
results_files <- dir("data/Essex/Screen_info/")

# setup data_frame 
df_screen_info <- data.frame(Participant = character(),
                          x_res = numeric(),
                          y_res = numeric())

df_deltas <- data.frame(Participant = character(),
                     Delta = numeric())

df_beta <- data.frame(Participant = character(),
                        beta1 = numeric(),
                        beta2 = numeric())

df_avatar_info <- data.frame(Participant = character(),
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
  df_screen_info <- rbind(df_screen_info, data.frame(Participant = Participant,
                                               x_res = x_res,
                                               y_res = y_res))
  # deltas 
  df_deltas <- rbind(df_deltas, data.frame(Participant = Participant, 
                                           Delta = Delta))
  # beta
  df_beta <- rbind(df_beta, data.frame(Participant = Participant,
                                       beta1 = beta1,
                                       beta2 = beta2))
  
  # avatar 
  df_avatar_info <- rbind(df_avatar_info, data.frame(Participant = Participant,
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
df_decisions <- data.frame(Participant = character(),
                           Condition = character(),
                           Spread = character(),
                           Block = numeric(),
                           Trial = numeric(),
                           Delta = numeric(),
                           RT = numeric(),
                           Initial_X = numeric(),
                           Placed_X = numeric(),
                           Target_side = numeric(),
                           Speed = numeric(),
                           Success = numeric())

# loop through files 
for(f in results_files){
  # read in file
  d <- read.csv(paste("data/Essex/Decisions/", f, sep = ""), header = T)
  
  # Participant 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[1]
  
  # add in Participant
  d$Participant <- Participant
  
  # combine data
  df_decisions <- rbind(df_decisions, d)
}

# tidy 
rm(d, f, Participant, results_files)

# arrange this 
df_decisions <- select(df_decisions,
                       Participant,
                       everything())

# add in an order variable 
# empty vector
Rand_first <- c()

# loop through to classify
for(p in unique(df_decisions$Participant)){
  # get subset
  ss <- df_decisions[df_decisions$Participant == p,] 
  
  # get first block condition
  first_cond <- ss$Spread[ss$Block == 1 & ss$Trial == 1]
  # check block 
  if(first_cond == 1){
    rf <- 1
  } else {
    rf <- 0
  }
  
  # add to empty vector
  Rand_first <- c(Rand_first, rep(rf, nrow(ss)))
}

# add to data frame
df_decisions$Rand_first <- Rand_first

# tidy
rm(first_cond, p, Rand_first, rf, ss)

# labels for spread 
df_decisions$truck_perf <- "Random_Uniform"
df_decisions$truck_perf[df_decisions$Spread > 1] = "Highly_Certain"

# add in a standard sep 
for(p in unique(df_decisions$Participant)){
  df_decisions$standard[df_decisions$Participant == p] <- as.numeric(as.factor(df_decisions$Delta[df_decisions$Participant == p]))
}

rm(p)

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
df_estimates <- c(Participant = character(),
                  Delta = numeric(),
                  Estimate = numeric())

# loop to read 
for(f in results_files){
  # read file
  d <- read.csv(paste("data/Essex/Estimates/", f, sep = ""), header = T)
  
  # Participant 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[1]
  
  # add in Participant
  d$Participant <- Participant
  
  # bind to data frame
  df_estimates <- rbind(df_estimates, d)
}

# tidy
rm(d, f, Participant, results_files)

# arrange 
df_estimates <- select(df_estimates,
                       Participant,
                       everything())

# add in a label to use later 
df_estimates$Estimate_Type <- "Participant"

# add in truck_perf
df_estimates$truck_perf <- "Random_Uniform"
df_estimates$truck_perf[df_estimates$Spread > 1] = "Highly_Certain"


# save this file 
save(df_estimates, file = "scratch/data/df_estimates")


#### Read in click history ####
# set path 
results_files <- dir("data/Essex/Click_history/")

# empty frame
df_clickhist <- data.frame(Participant = character(),
                           Block = numeric(),
                           Trial = numeric(),
                           Time_taken = numeric(),
                           Delta = numeric(),
                           Num_clicks = numeric(),
                           Placed_x = numeric())

# loop to read in data 
for(f in results_files){
  # read file
  d <- read.csv(paste("data/Essex/Click_history/", f, sep = ""), header = T)
  
  # Participant 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[1]
  
  # add in Participant
  d$Participant <- Participant
  
  # bind 
  df_clickhist <- rbind(df_clickhist, d)
}

# tidy 
rm(d, f, Participant, results_files)

# arrange 
df_clickhist <- select(df_clickhist,
                       Participant, 
                       everything())

# save this
save(df_clickhist, file = "scratch/data/df_clickhist")

#### Read in Demo_phase ####
results_files <- dir("data/Essex/Demo_phase/")

# empty frame
df_demo_phase <- data.frame(Participant = character(),
                            Trial = numeric(),
                            Success = numeric())

for(f in results_files){
  # make file
  d <- read.csv(paste("data/Essex/Demo_phase/", f, sep = ""), header = T)
  
  # Participant 
  Participant <- strsplit(f, '[_.]')[[1]]
  Participant <- Participant[1]
  
  # add in Participant
  d$Participant <- Participant
  
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
df_demo_phase$truck_perf <- "Random_Uniform"
df_demo_phase$truck_perf[df_demo_phase$Spread > 1] = "Highly_Certain"

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

# save 
save(df_confidence, file = "scratch/data/df_confidence")

# clear environment 
rm(list = ls())




