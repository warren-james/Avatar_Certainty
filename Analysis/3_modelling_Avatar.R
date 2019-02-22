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
load("scratch/data/df_decisions")

# make model data 
model_data <- df_decisions %>%
  mutate(Abs_Norm_pos = abs(Placed_x/Delta))

# add in binary predictors for stan modelling 
# condition
model_data$cnd_rand <- 1
model_data$cnd_rand[model_data$truck_perf == "Highly_Certain"] <- 0

# reduce down columns 
model_data <- model_data %>%
  select(-Condition,
         -Spread, 
         -Initial_x,
         -Speed,
         -Success,
         -standard) %>% # only for now
  mutate(Norm_Delta = Delta/max(Delta)) %>%
  filter(Abs_Norm_pos < 1.01)


# load in the estimates data 
load("scratch/data/df_estimates")

#### pre-analysis #### 
# first want to look at the correlation of estimates accross condition 
# within in each participant 

# so sort that 
temp <- df_estimates %>%
  filter(Estimate_Type == "Participant") %>%
  group_by(Participant, truck_perf, Delta) %>%
  summarise(Estimate = mean(Estimate)) %>%
  spread(truck_perf, Estimate) %>%
  ggplot(aes(Random_Uniform, Highly_Certain, 
             colour = Delta)) + 
  geom_point() + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = F) + 
  facet_wrap(~Participant)
 temp 

# get the R^2 values 
for(p in unique(df_estimates$Participant)){
  # subset
  ss <- df_estimates[df_estimates$Participant == p,]
  # linear model 
  check_cor <- glm(Estimate ~ (Delta + truck_perf)^2,
                   data = ss,
                   family = "binomial")
  # get R^2 value
  rsqr <- summary(check_cor)$r.squared
  print(rsqr)
  
}
#### Models ####
#### Placement as predicted variable ####
#### place_m1: Norm_placement ~ Delta ####
# Use normalised Delta to predict Placement 
# nothing else, just the one predictor 

# quick brms version 
model_brms_1 <- brm(Abs_Norm_pos ~ Norm_Delta,
                    data = model_data,
                    family = "beta",
                    iter = 2000,
                    chains = 1,
                    cores = 1)

#### place_m2: Norm_placement ~ Delta + Condition ####
# add in the condition variable as a main effect 

# quick brms version 
model_brms_2 <- brm(Abs_Norm_pos ~ Norm_Delta + truck_perf,
                    data = model_data,
                    family = "beta",
                    iter = 2000,
                    chains = 1,
                    cores = 1)


#### place_m3: Norm_placement ~ (Delta + Condition)^2 ####
# main effects and interactions of Delta and Condition

# quick brms version 
model_brms_3<- brm(Abs_Norm_pos ~ (Norm_Delta + truck_perf)^2,
                   data = model_data,
                   family = "beta",
                   iter = 2000,
                   chains = 1,
                   cores = 1)


#### place_m4: Norm_placement ~ Delta + Condition + Order ####
# add in the main effect of order



#### place_m5: Norm_placement ~ (Delta + Condition + Order)^2 ####
# add in all two way interactions


#### Opt choice models ####
#### opt_m1: opt ~ Delta ####