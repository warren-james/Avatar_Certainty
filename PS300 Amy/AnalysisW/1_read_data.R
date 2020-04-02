#### import data #### 
# this script will import all the data for the Certainty version of the avatar expriment

#### Library ####
library(tidyverse)
library(R.matlab)

#### Read in data ####
#### decisions #### 
results_files <- c("data/Decisions/")

df_decisions <- tibble()

for(f in dir(results_files)){
  d <- read.csv(paste(results_files, f, sep = ""))
  d$Participant <- strsplit(f, '[_.]')[[1]][1] 
  df_decisions <- rbind(df_decisions, d)
}

# tidy 
rm(f, results_files, d)

# tidy dataset 
df_decisions <- df_decisions %>% 
  filter(!Participant %in% c("0"))

# save file
save(df_decisions, file = "scratch/df_decisions")



#### demographics ####
results_files <- c("data/Demographics/")

df_demographics <- tibble() 

for(f in dir(results_files)) {
  d <- readMat(paste(results_files, f, sep = ""))
  age <- d$age
  gender <- d$gender
  participant <- strsplit(f, '[_.]')[[1]][1]
  df_demographics <- rbind(df_demographics, data.frame(Participant = participant, 
                                                       age = age,
                                                       gender = gender))
}

# tidy data 
df_demographics <- df_demographics %>% 
  mutate(gender = tolower(gender),
         gender = ifelse(gender == "f", "female", gender)) 

# save 
save(df_demographics, file = "scratch/df_demographics")

# tidy 
rm(age, gender, f, participant, results_files, d)


#### confidence #### 
results_files <- c("data/Confidence/")

df_confidence <- tibble()

for(f in dir(results_files)) {
  d <- read.csv(paste(results_files, f, sep = ""))
  d$Participant <- strsplit(f, '[_.]')[[1]][1]
  
  df_confidence <- rbind(df_confidence, d)
}

# tidy data
df_confidence <- df_confidence %>% 
  filter(Participant != "0")

# save 
save(df_confidence, file = "scratch/df_confidence")

# tidy 
rm(d, f, results_files)


#### screen_info ####
results_files <- c("data/Screen_info/")

# setup df
df_screen_info <- tibble(Participant = character(),
                         x_res = numeric(),
                         y_res = numeric())

df_deltas <- tibble(Participant = character(),
                    delta = numeric())

df_beta <- tibble(Participant = character(),
                  beta1 = numeric(),
                  beta2 = numeric())

df_avatar_info <- tibble(Participant = character(),
                         max_speed = numeric(),
                         reach = numeric())

for(f in dir(results_files)) {
  d <- readMat(paste(results_files, f, sep = ""))
  # participant
  Participant <- strsplit(f, '[_.]')[[1]][1]
  # screen res 
  x_res <- as.numeric(d$params[1]) * 2
  y_res <- as.numeric(d$params[2]) * 2
  # betas
  betas <- d$box[[2]]
  beta1 <- betas[1]
  beta2 <- betas[2]
  # Delta 
  delta <- unlist(d$params[3])
  # reach
  reach <- as.numeric(d$box[4])
  # max_speed
  max_speed <- as.numeric(d$box[1])
  
  # bind to dataframes 
  df_screen_info <- rbind(df_screen_info, tibble(Participant = Participant,
                                                 x_res = x_res,
                                                 y_res = y_res))
  df_deltas <- rbind(df_deltas, tibble(Participant = Participant, 
                                       delta = delta))
  df_beta <- rbind(df_beta, tibble(Participant = Participant, 
                                   beta1 = beta1,
                                   beta2 = beta2))
  df_avatar_info <- rbind(df_avatar_info, tibble(Participant = Participant,
                                                 max_speed = max_speed,
                                                 reach = reach))
}

# tidy 
rm(betas, d, beta1, beta2, delta, f, max_speed,
   Participant, reach, results_files, x_res, y_res)

# save 
save(df_avatar_info, file = "scratch/df_avatar_info")
save(df_screen_info, file = "scratch/df_screen_info")

#### click history #### 
# participant 16 doesn't have any data here for some reason
results_files <- c("data/Click_history/")

df_clickhist <- tibble()

for(f in dir(results_files)) {
  if(f == "16_clickhist.txt") {
    break
  }
  d <- read.csv(paste(results_files, f, sep = ""))
  d$Participant <- strsplit(f, '[_.]')[[1]][1]
  df_clickhist <- rbind(df_clickhist, d)
}

# tidy 
rm(d, f, results_files)

# save 
save(df_clickhist, file = "scratch/df_clickhist")
