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
load("scratch/data/df_Aberdeen_decisions")

# make model data 
model_data <- df_Aberdeen_decisions %>%
  mutate(Abs_Norm_pos = abs(placed_x/delta)) 

# add in binary predictors for stan modelling 
# condition
model_data$cnd_rand <- 1
model_data$cnd_rand[model_data$truck_perf == "Constant"] <- 0

# reduce down columns 
model_data <- model_data %>%
  select(-condition,
         -spread, 
         -initial_x,
         -speed) %>% # only for now
  mutate(Norm_Delta = delta/max(delta)) %>%
  filter(Abs_Norm_pos < 1 + 1e-8) %>% 
  mutate(Abs_Norm_pos = (Abs_Norm_pos + 1e-5)*0.9999) # need to ensure the predicted variable is between 0 and 1 

# add in dist_type 
model_data$dist_type <- "close"
model_data$dist_type[model_data$Norm_Delta > median(model_data$Norm_Delta)] <- "far"

# make truck only 
model_truckonly <- model_data %>% 
  filter(condition_label == "truck")

#### Models ####
#### Placement as predicted variable ####
#### place_m1: Norm_placement ~ Delta ####
# Use normalised Delta to predict Placement 
# nothing else, just the one predictor 

# quick brms version 
place_real_brms_1 <- brm(Abs_Norm_pos ~ Norm_Delta,
                         data = model_data,
                         family = "beta",
                         iter = 2000,
                         chains = 1,
                         cores = 1)

# save this 
save(place_real_brms_1, file = "models/outputs/brms/Real/place_real_brms_1")

#### place_m1.1: add in rand intercepts ####
place_real_brms_1.1 <- brm(Abs_Norm_pos ~ Norm_Delta + (1|participant),
                           data = model_data,
                           family = "beta",
                           iter = 2000,
                           chains = 1,
                           cores = 1)

# save this 
save(place_real_brms_1.1, file = "models/outputs/brms/place_real_brms_1.1")

#### place_m1.2: add in rand slopes ####
place_real_brms_1.2 <- brm(Abs_Norm_pos ~ Norm_Delta + (1 + Norm_Delta|participant),
                           data = model_data,
                           family = "beta",
                           iter = 2000,
                           chains = 1,
                           cores = 1)

# save this 
save(place_real_brms_1.2, file = "models/outputs/brms/place_real_brms_1.2")

#### place_m2: Norm_placement ~ Delta + Condition ####
# add in the condition variable as a main effect 

# quick brms version 
place_real_brms_2 <- brm(Abs_Norm_pos ~ Norm_Delta + truck_perf,
                         data = model_data,
                         family = "beta",
                         iter = 2000,
                         chains = 1,
                         cores = 1)

# save 
save(place_real_brms_2, file = "models/outputs/brms/Real/place_real_brms_2")

#### place_m2.1: add in rand intercepts ####
place_real_brms_2.1 <- brm(Abs_Norm_pos ~ Norm_Delta + truck_perf + (1|participant),
                           data = model_data,
                           family = "beta",
                           iter = 2000,
                           chains = 1,
                           cores = 1)

#### place_m2.2: add in rand slopes ####
place_real_brms_2.2 <- brm(Abs_Norm_pos ~ Norm_Delta + truck_perf + (1 + Norm_Delta|participant),
                           data = model_data,
                           family = "beta",
                           iter = 2000,
                           chains = 1,
                           cores = 1)


#### place_m3: Norm_placement ~ (Delta + Condition)^2 ####
# main effects and interactions of Delta and Condition

# quick brms version 
place_real_brms_3 <- brm(Abs_Norm_pos ~ (Norm_Delta + truck_perf)^2,
                         data = model_data,
                         family = "beta",
                         iter = 2000,
                         chains = 1,
                         cores = 1)

# version just on truck part 
model_truckonly <- model_data[model_data$condition_label == "truck",]

# run model
place_compare_brms_3 <- brm(Abs_Norm_pos ~ (Norm_Delta + truck_perf)^2,
                            data = model_truckonly,
                            family = "beta",
                            chains = 1,
                            iter = 2000,
                            cores = 1)

# add priors
place_compare_brms_3_priors <- brm(Abs_Norm_pos ~ (Norm_Delta + truck_perf)^2,
                                   data = model_truckonly,
                                   family = "beta",
                                   prior = c(set_prior("normal(2.77, 1.5)",
                                                       class = "b",
                                                       coef = "Norm_Delta"),
                                             set_prior("normal(-0.39, 1)",
                                                       class = "b",
                                                       coef = "Norm_Delta:truck_perfVariable"),
                                             set_prior("normal(0.66, 1)",
                                                       class = "b",
                                                       coef = "truck_perfVariable"),
                                             set_prior("normal(-2.55, 1)",
                                                       class = "Intercept")),
                                   chains = 1, iter = 2000, cores = 1)


# save these
save(place_compare_brms_3, file = "models/outputs/brms/Real/place_compare_brms_3")
save(place_compare_brms_3_priors, file = "models/outputs/brms/Real/place_compare_brms_3_priors")

#### place_m3_v2: using dist_type ####
place_compare_brms_3_v2 <- brm(Abs_Norm_pos ~ (dist_type + truck_perf)^2,
                               data = model_truckonly,
                               family = "beta",
                               chains = 1,
                               iter = 2000,
                               cores = 1)

# add in priors
place_compare_brms_3_priors_v2 <- brm(Abs_Norm_pos ~ (dist_type + truck_perf)^2,
                                      data = model_truckonly,
                                      family = "beta",
                                      prior = c(set_prior("normal(1.71, 1.5)",
                                                          class = "b",
                                                          coef = "dist_typefar"),
                                                set_prior("normal(-0.29, 1.5)",
                                                          class = "b",
                                                          coef = "dist_typefar:truck_perfVariable"),
                                                set_prior("normal(0.48, 1.5)",
                                                          class = "b",
                                                          coef = "truck_perfVariable"),
                                                set_prior("normal(-1.71, 1.5)",
                                                          class = "Intercept")),
                                      chains = 1, iter = 2000, cores = 1)

# save 
save(place_compare_brms_3_v2, file = "models/outputs/brms/Real/place_compare_brm_3_v2")
save(place_compare_brms_3_priors_v2, file = "models/outputs/brms/Real/place_compare_brm_3_priors_v2")

#### place_m3.1: add in rand intercepts ####
place_real_brms_3.1 <- brm(Abs_Norm_pos ~ (Norm_Delta + truck_perf)^2 + (1|participant),
                           data = model_data,
                           family = "beta",
                           iter = 2000,
                           chains = 1,
                           cores = 1)

#### place_m3.2: add in rand slopes ####
place_real_brms_3.2 <- brm(Abs_Norm_pos ~ (Norm_Delta + truck_perf)^2 +
                             (1 + Norm_Delta*truck_perf|participant),
                           data = model_data,
                           family = "beta",
                           iter = 2000,
                           chains = 1,
                           cores = 1)

# same again but with using num_speed
# model_brms_3.1 <- brm(Abs_Norm_pos ~ (Norm_Delta + num_speeds)^2,
#                      data = model_data,
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
#                     data = model_data, 
#                     family = "beta",
#                     iter = 2000,
#                     chains = 1,
#                     cores = 1)

#### place_m4: Norm_placement ~ Delta + Condition + condition_label ####
# add in the main effect of order
place_real_brms_4 <- brm(Abs_Norm_pos ~ Norm_Delta + truck_perf + condition_label, 
                         data = model_data, 
                         family = "beta",
                         iter = 2000,
                         chains = 1,
                         cores = 1)

# save 
save(place_real_brms_4, file = "models/outputs/brms/Real/place_real_brms_4")


#### place_m5: Norm_placement ~ (Delta + truck_perf + condition)^2 ####
# add in all two way interactions
place_real_brms_5 <- brm(Abs_Norm_pos ~ (Norm_Delta + truck_perf + condition_label)^2, 
                         data = model_data, 
                         family = "beta",
                         iter = 2000,
                         chains = 1,
                         cores = 1)

# save 
save(place_real_brms_5, file = "models/outputs/brms/Real/place_real_brms_5")

#### place_m6: Norm_placement ~ (Delta + truck_perf + condition)^3 ####
place_real_brms_6 <- brm(Abs_Norm_pos ~ (Norm_Delta + truck_perf + condition_label)^3, 
                         data = model_data, 
                         family = "beta",
                         iter = 2000,
                         chains = 1,
                         cores = 1)

# save 
save(place_real_brms_6, file = "models/outputs/brms/Real/place_real_brms_6")


#### ACCURACY ####
# modelling of Accuracy... 
# will work on a stan version but for now here's a quick brms idea
acc_brms <- brm(success ~ (Norm_Delta + truck_perf + condition_label)^3,
                data = model_data,
                family = "bernoulli",
                iter = 2000,
                chains = 1,
                cores = 1)


# could simplify to just average accuracy and use a beta dist?
model_acc_data <- model_data %>%
  group_by(participant, dist_type, truck_perf, condition_label) %>%
  summarise(Accuracy = mean(success)) %>%
  mutate(Accuracy = (Accuracy + 1e-5)*0.9999)

acc_brms_summ <- brm(Accuracy ~ (dist_type + truck_perf + condition_label)^3,
                     data = model_acc_data, 
                     family = "beta",
                     iter = 2000,
                     chains = 1,
                     cores = 1)

# save 
save(acc_brms_summ, file = "models/outputs/brms/Real/acc_brms_summ")

#### NB: need to look at the expected accuracy as well... ####
# now, being at the side does entail 50% accuracy... so we need to account for that
model_chance_data <- model_data %>%
  group_by(participant, dist_type, truck_perf, condition_label) %>%
  summarise(chance = mean(chance)) %>%
  mutate(chance = (chance + 1e-5)*0.9999)

# model 
chance_brms_summ <- brm(chance ~ (dist_type + truck_perf + condition_label)^3,
                        data = model_chance_data,
                        family = "beta",
                        iter = 2000,
                        chains = 1,
                        cores = 1)

# save 
save(chance_brms_summ, file = "models/outputs/brms/Real/chance_brms_summ")


