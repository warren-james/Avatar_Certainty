#### Avatar - Analysis script ####
# few ideas
# modelling optimal choice vs. not 
# - Define a range for each distance that would get the optimal 
#   Accuracy and simply model if people are in that range for each
#   condition at the various distances?
# - Could also just do mean normalised placement again with order 
#   and condition as predictors?
# - What about some more simple analysis for the final years working 
#   with Alasdair?


#### Library ####
library(tidyverse)
library(rstan)
library(brms) # probably won't need this...

#### Notes ####
# max speed is max(df_deltas)/100
# for Condition, 1 = Avatar, 2 = Truck
# for Spread 1 = Randunif, 2 = Hard cutoff

# do we ever want to use RT as a predictor?
# might make sense to do this and centre it on the global average?

#### Constants ####
travel_time <- 100

#### Any Functions ####

#### Load in data ####
load("scratch/data/df_Essex_decisions")

# make model data 
model_pilot_data <- df_Essex_decisions %>%
  mutate(Abs_Norm_pos = abs(placed_x/delta)) %>%
  group_by(truck_perf) %>%
  mutate(num_speeds = length(unique(speed))) %>%
  ungroup()

# add in binary predictors for stan modelling 
# condition
model_pilot_data$cnd_rand <- 1
model_pilot_data$cnd_rand[model_pilot_data$truck_perf == "Constant"] <- 0

# reduce down columns 
model_pilot_data <- model_pilot_data %>%
  select(-condition,
         -spread, 
         -initial_x,
         -speed,
         -success) %>% # only for now
  mutate(Norm_Delta = delta/max(delta)) %>%
  filter(Abs_Norm_pos < 1.01)


# load in the estimates data 
load("scratch/data/df_estimates")

#### pre-analysis #### 
# first want to look at the correlation of estimates accross condition 
# within in each participant 

# get a value to show how similar slopes are....
# This models each participants estimates for the different 
# conditions, then gets a value about how similar they are.
# numbers closer to 0 better? 
# setup data frame to record this 
df_slope_diff <- data.frame(participant = character(),
                            difference = numeric())

# run loop
for(p in unique(df_estimates$participant)){
  # subset
  ss <- df_estimates[df_estimates$participant == p,]
  ss$delta2 <- ss$delta/max(ss$delta)
  # make quick model 
  model <- glm(estimate ~ truck_perf * delta2,
               data = ss,
               family = "binomial")
  
  # get estimate of different fits
  m.model <- lsmeans::lstrends(model, "truck_perf", var = "delta2")
  
  # get number to show how well correlated the estimates are 
  num <- summary(pairs(m.model))
  num <- num$estimate
  
  # store this 
  df_slope_diff <- rbind(df_slope_diff, data.frame(participant = p,
                                                   difference = num))
}

# plt just as a quick check 
df_estimates %>%
  ggplot(aes(delta,
             estimate,
             colour = truck_perf)) +
  geom_smooth(method = glm,
              method.args = list(family = "binomial"),
              se = F) +
  facet_wrap(~participant)

# probably want to combine this with the model data so it can be 
# used as a predictor... if we decide to use it?

# add in to model_pilot_data 
model_pilot_data <- merge(model_pilot_data, df_slope_diff)

# save this 
save(model_pilot_data, file = "scratch/data/model_pilot_data")

#### Models ####
#### Placement as predicted variable ####
#### place_m1: Norm_placement ~ Delta ####
# Use normalised Delta to predict Placement 
# nothing else, just the one predictor 

# quick brms version 
place_pilot_brms_1 <- brm(Abs_Norm_pos ~ Norm_Delta,
                          data = model_pilot_data,
                          family = "beta",
                          iter = 2000,
                          chains = 1,
                          cores = 1)

# save this 
save(place_pilot_brms_1, file = "models/outputs/brms/place_pilot_brms_1")

#### place_m1.1: add in rand intercepts ####
place_pilot_brms_1.1 <- brm(Abs_Norm_pos ~ Norm_Delta + (1|participant),
                            data = model_pilot_data,
                            family = "beta",
                            iter = 2000,
                            chains = 1,
                            cores = 1)

# save this 
save(place_pilot_brms_1.1, file = "models/outputs/brms/place_pilot_brms_1.1")

#### place_m1.2: add in rand slopes ####
place_pilot_brms_1.2 <- brm(Abs_Norm_pos ~ Norm_Delta + (1 + Norm_Delta|participant),
                            data = model_pilot_data,
                            family = "beta",
                            iter = 2000,
                            chains = 1,
                            cores = 1)

# save this 
save(place_pilot_brms_1.2, file = "models/outputs/brms/place_pilot_brms_1.2")

#### place_m2: Norm_placement ~ Delta + Condition ####
# add in the condition variable as a main effect 

# quick brms version 
place_pilot_brms_2 <- brm(Abs_Norm_pos ~ Norm_Delta + truck_perf,
                          data = model_pilot_data,
                          family = "beta",
                          iter = 2000,
                          chains = 1,
                          cores = 1)


#### place_m2.1: add in rand intercepts ####
place_pilot_brms_2.1 <- brm(Abs_Norm_pos ~ Norm_Delta + truck_perf + (1|participant),
                            data = model_pilot_data,
                            family = "beta",
                            iter = 2000,
                            chains = 1,
                            cores = 1)

#### place_m2.2: add in rand slopes ####
place_pilot_brms_2.2 <- brm(Abs_Norm_pos ~ Norm_Delta + truck_perf + (1 + Norm_Delta|participant),
                            data = model_pilot_data,
                            family = "beta",
                            iter = 2000,
                            chains = 1,
                            cores = 1)


#### place_m3: Norm_placement ~ (Delta + Condition)^2 ####
# main effects and interactions of Delta and Condition

# quick brms version 
place_pilot_brms_3 <- brm(Abs_Norm_pos ~ (Norm_Delta + truck_perf)^2,
                          data = model_pilot_data,
                          family = "beta",
                          iter = 2000,
                          chains = 1,
                          cores = 1)

# save 
save(place_pilot_brms_3, file = "models/outputs/brms/Pilot/place_pilot_brms_3")

#### place_m3.1: add in rand intercepts ####
place_pilot_brms_3.1 <- brm(Abs_Norm_pos ~ (Norm_Delta + truck_perf)^2 + (1|participant),
                            data = model_pilot_data,
                            family = "beta",
                            iter = 2000,
                            chains = 1,
                            cores = 1)

#### place_m3.2: add in rand slopes ####
place_pilot_brms_3.2 <- brm(Abs_Norm_pos ~ (Norm_Delta + truck_perf)^2 +
                              (1 + Norm_Delta*truck_perf|participant),
                            data = model_pilot_data,
                            family = "beta",
                            iter = 2000,
                            chains = 1,
                            cores = 1)

# same again but with using num_speed
# model_brms_3.1 <- brm(Abs_Norm_pos ~ (Norm_Delta + num_speeds)^2,
#                      data = model_pilot_data,
#                      family = "beta",
#                      iter = 2000,
#                      chains = 1,
#                      cores = 1)

#### temp model: Norm_place ~ (Delta + Condition)^2 + Condition*difference ####
# not sure if this is the right way to do this... 
# we're more interested in the difference of the slope... right? 
# maybe we want the 3 way interaction?
# quick brms version
# model_brms_4 <- brm(Abs_Norm_pos ~ (Norm_Delta + truck_perf)^2 + truck_perf*Norm_Delta*difference, 
#                     data = model_pilot_data, 
#                     family = "beta",
#                     iter = 2000,
#                     chains = 1,
#                     cores = 1)

#### place_m4: Norm_placement ~ Delta + Condition + Order ####
# add in the main effect of order



#### place_m5: Norm_placement ~ (Delta + Condition + Order)^2 ####
# add in all two way interactions


#### Opt choice models ####
#### opt_m1: opt ~ Delta ####