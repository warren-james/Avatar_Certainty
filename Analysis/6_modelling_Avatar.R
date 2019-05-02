#### Avatar - Analysis script ####

#### Library ####
library(brms) 
library(rethinking)
library(rstan)
library(tidybayes)
library(tidyverse)

#### Notes ####
# max speed is max(df_deltas)/100
# for Condition, 1 = Avatar, 2 = Truck
# for Spread 1 = Randunif, 2 = Hard cutoff

# do we ever want to use RT as a predictor?
# might make sense to do this and centre it on the global average?

#### Constants ####
travel_time <- 100

#### Any Functions ####
# plotting mean estimates
STAN_plt <- function(model_output, dataframe, effs){
  # setup plt data
  plt_data <- as.tibble(model_output) %>%
    gather(key = "remove",
           value = "pred_mu") %>%
    group_by(remove) %>%
    mutate(row_num = strsplit(remove, split = "V")[[1]][2]) %>%
    ungroup() %>%
    select(-remove) %>%
    merge(dataframe)
  # make plt to output
  output <- plt_data %>%
    ggplot(aes(pred_mu,
               colour = truck_perf,
               fill = truck_perf)) + 
    geom_density(alpha = 0.3) + 
    theme_minimal() + 
    theme(legend.position = "bottom") + 
    ggthemes::scale_color_ptol() + 
    ggthemes::scale_fill_ptol()
  if(effs == 2){
    output <- output + facet_wrap(~plt_data[[4]])
  } else if(effs == 3){
    output <- output + facet_grid(plt_data[[4]]~plt_data[[5]])
  }
  return(output)
}


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
  mutate(Abs_Norm_pos = (Abs_Norm_pos + 1e-5)*0.9999,
         dist_type = ifelse(Norm_Delta > median(Norm_Delta), "far", "close"))  

# make truck only 
model_truckonly <- model_data %>% 
  filter(condition_label == "truck")

#### BRM Models ####
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



#### STAN Models ####
#### STAN: Placement ####
# First, do everything we're interested in, so all interactions...
X <- model.matrix(Abs_Norm_pos ~ (dist_type + truck_perf + condition_label)^3,
                  data = model_data)

# add in row identifier
model_data <- model_data %>%
  rownames_to_column(var = "row_num") %>% 
  select(row_num, truck_perf, condition_label, dist_type, Abs_Norm_pos)

stan_df <- list(
  N = nrow(model_data),
  K = ncol(X),
  y = model_data$Abs_Norm_pos,
  X = X
)

m_stan_full_pos <- stan(
  file = "models/stan_files/stan_model.stan",
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

samples <- rstan::extract(m_stan_full_pos)
plt_stan_full_pos <- STAN_plt(samples$mu, model_data, effs = 3)
plt_stan_full_pos$labels$x <- "Predicted Mean Placement Position"
plt_stan_full_pos$labels$colour <- "Truck Performance"
plt_stan_full_pos$labels$fill <- "Truck Performance"
plt_stan_full_pos

# get HPDI stuff 
HPDI_sfp <- plt_stan_full_pos[["data"]] %>%
  group_by(truck_perf, condition_label, dist_type) %>%
  summarise(mean_est = mean(pred_mu),
            lower = HPDI(pred_mu, 0.95)[1],
            upper = HPDI(pred_mu, 0.95)[2])
HPDI_sfp

# try posterior predictions? 
post_preds <- function(m, pred_dat, x){
  post <- rstan::extract(m)
  
  beta <- colMeans(post$beta)
  gamma <- colMeans(post$gamma)
  
  mu  <- plogis(X %*% beta)
  phi <- exp(X %*% gamma)
  
  A <- mu * phi 
  B <- (1 - mu) * phi
  
  n <- length(x)
  
  p <- unlist(map2(A, B, dbeta, x = x))
  
  return(p)
}