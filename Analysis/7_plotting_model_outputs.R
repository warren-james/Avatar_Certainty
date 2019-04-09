#### plotting model outputs ####
# probably only need to load in the models I care about?
# 1st: Compare the different experiments 
#      This includes looking at the difference between the two with flat priors 
#      and using the model outputs from Essex to see how fit improves
# 2nd: Look at difference between abstract and truck versions 
# 3rd: Compare Group and Individual versions (confounded by Aberdeen vs. Essex... but might be interesting)
# 4th: Models using dist_type instead of norm_delta
# 5th: Accuracy and expected accuracy 

#### Library ####
library(tidyverse)
library(brms)
library(tidybayes)
library(rstan)

#### Load data ####
load("scratch/data/df_Aberdeen_decisions")

# make model data 
model_data_real <- df_Aberdeen_decisions %>%
  mutate(Abs_Norm_pos = abs(placed_x/delta)) 

# add in binary predictors for stan modelling 
# condition
model_data_real$cnd_rand <- 1
model_data_real$cnd_rand[model_data_real$truck_perf == "Constant"] <- 0

# reduce down columns 
model_data_real <- model_data_real %>%
  select(-condition,
         -spread, 
         -initial_x,
         -speed) %>% # only for now
  mutate(Norm_Delta = delta/max(delta)) %>%
  filter(Abs_Norm_pos < 1 + 1e-8) %>% 
  mutate(Abs_Norm_pos = (Abs_Norm_pos + 1e-5)*0.9999) # need to ensure the predicted variable is between 0 and 1 

# add in dist_type 
model_data_real$dist_type <- "close"
model_data_real$dist_type[model_data_real$Norm_Delta > median(model_data_real$Norm_Delta)] <- "far"

# make truck only 
model_truckonly <- model_data_real %>% 
  filter(condition_label == "truck")

# load Essex
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
  filter(Abs_Norm_pos < 1.01) %>% 
  mutate(Abs_Norm_pos = (Abs_Norm_pos + 1e-5)*0.9999) 

# add in dist_type 
model_pilot_data$dist_type <- "close"
model_pilot_data$dist_type[model_pilot_data$Norm_Delta > median(model_pilot_data$Norm_Delta)] <- "far"
model_pilot_data$dist_type[model_pilot_data$Norm_Delta == median(model_pilot_data$Norm_Delta)] <- "midpoint"

model_pilot_data_midremoved <- model_pilot_data %>%
  filter(dist_type != "midpoint")

#### 1st: Essex and Aberdeen ####
#### 1st: Load models ####
load("models/outputs/brms/Pilot/place_pilot_brms_3")
load("models/outputs/brms/Real/place_compare_brms_3")
load("models/outputs/brms/Real/place_compare_brms_3_priors")

#### 1st: Plotting ####
# get marginal_effects 
post_pilot <- marginal_effects(place_pilot_brms_3)
post_compare <- marginal_effects(place_compare_brms_3)
post_compare_priors <- marginal_effects(place_compare_brms_3_priors)

plot(post_pilot)
plot(post_compare)
plot(post_compare_priors)

# posterior predictions 
plt_real <- model_truckonly %>%
  add_predicted_draws(place_compare_brms_3) %>%
  ggplot(aes(.prediction, colour = truck_perf, fill = truck_perf)) +
  geom_density(alpha = 0.3) +
  geom_density(data = model_truckonly,
               aes(Abs_Norm_pos,
                   colour = truck_perf,
                   fill = NA),
               alpha = 0.0001) +
  theme_minimal() +
  ggthemes::scale_colour_ptol() +
  ggthemes::scale_fill_ptol() +
  facet_wrap(~delta)
plt_real

plt_pilot <- model_pilot_data %>%
  add_predicted_draws(place_pilot_brms_3) %>%
  ggplot(aes(.prediction, colour = truck_perf, fill = truck_perf)) +
  geom_density(alpha = 0.3) +
  geom_density(data = model_pilot_data,
               aes(Abs_Norm_pos,
                   colour = truck_perf,
                   fill = NA),
               alpha = 0.0001) +
  theme_minimal() +
  ggthemes::scale_colour_ptol() +
  ggthemes::scale_fill_ptol() +
  facet_wrap(~delta)
plt_pilot



#### 2nd ####

#### 3rd ####

#### 4th ####
#### 4th: Load models ####
load("models/outputs/brms/Pilot/place_pilot_brms_3_v2")
load("models/outputs/brms/Real/place_compare_brm_3_v2")
load("models/outputs/brms/Real/place_compare_brm_3_priors_v2")

#### 4th: Plotting ####
# don't care about the line plots

# posterior predictions
plt_real <- model_truckonly %>%
  add_predicted_draws(place_compare_brms_3_v2) %>%
  ggplot(aes(.prediction, colour = truck_perf, fill = truck_perf)) +
  geom_density(alpha = 0.3) +
  # geom_density(data = model_truckonly,
  #              aes(Abs_Norm_pos,
  #                  colour = truck_perf,
  #                  fill = NA),
  #              alpha = 0.0001) +
  theme_minimal() +
  ggthemes::scale_colour_ptol() +
  ggthemes::scale_fill_ptol() +
  facet_wrap(~dist_type)
plt_real

plt_pilot <- model_pilot_data_midremoved %>%
  add_predicted_draws(place_pilot_brms_3_v2) %>%
  ggplot(aes(.prediction, colour = truck_perf, fill = truck_perf)) +
  geom_density(alpha = 0.3) +
  # geom_density(data = model_pilot_data,
  #              aes(Abs_Norm_pos,
  #                  colour = truck_perf,
  #                  fill = NA),
  #              alpha = 0.0001) +
  theme_minimal() +
  ggthemes::scale_colour_ptol() +
  ggthemes::scale_fill_ptol() +
  facet_wrap(~dist_type)
plt_pilot

plt_priors <- model_truckonly %>%
  add_predicted_draws(place_compare_brms_3_priors_v2) %>%
  ggplot(aes(.prediction, colour = truck_perf, fill = truck_perf)) +
  geom_density(alpha = 0.3) +
  # geom_density(data = model_pilot_data,
  #              aes(Abs_Norm_pos,
  #                  colour = truck_perf,
  #                  fill = NA),
  #              alpha = 0.0001) +
  theme_minimal() +
  ggthemes::scale_colour_ptol() +
  ggthemes::scale_fill_ptol() +
  facet_wrap(~dist_type)
plt_priors


#### 5th ####
#### 5th: Load models ####
load("models/outputs/brms/Real/acc_brms_summ")
load("models/outputs/brms/Real/chance_brms_summ")

#### 5th: Sort data ####
model_acc_data <- model_data %>%
  group_by(participant, dist_type, truck_perf, condition_label) %>%
  summarise(Accuracy = mean(success)) %>%
  mutate(Accuracy = (Accuracy + 1e-5)*0.9999)

model_chance_data <- model_data %>%
  group_by(participant, dist_type, truck_perf, condition_label) %>%
  summarise(chance = mean(chance)) %>%
  mutate(chance = (chance + 1e-5)*0.9999)

#### 5th: plots ####
# posterior predictions
plt_acc <- model_acc_data %>%
  add_predicted_draws(acc_brms_summ) %>%
  ggplot(aes(.prediction, colour = truck_perf, fill = truck_perf)) +
  geom_density(aes(y = ..scaled..), alpha = 0.3) +
  # geom_density(data = model_acc_data,
  #              aes(Accuracy,
  #                  colour = truck_perf,
  #                  fill = NA),
  #              alpha = 0.0001) +
  theme_minimal() +
  ggthemes::scale_colour_ptol() +
  ggthemes::scale_fill_ptol() +
  facet_grid(condition_label ~ dist_type)
plt_acc

plt_chance <- model_chance_data %>%
  add_predicted_draws(chance_brms_summ) %>%
  ggplot(aes(.prediction, colour = truck_perf, fill = truck_perf)) +
  geom_density(aes(y = ..scaled..), alpha = 0.3) +
  # geom_histogram(position = "dodge") + 
  # geom_density(data = model_chance_data,
  #              aes(chance, y = ..scaled..,
  #                  colour = truck_perf,
  #                  fill = NA),
  #              alpha = 0.0001) +
  theme_minimal() +
  ggthemes::scale_colour_ptol() +
  ggthemes::scale_fill_ptol() +
  facet_grid(condition_label ~ dist_type)
plt_chance
