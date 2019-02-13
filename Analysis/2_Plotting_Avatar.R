#### Avatar - Make some plots ####
# plots of decisions 
# compare estimates to actual performance curves
# Something with number of clicks?
# RT may only be useful for modelling?

#### Notes ####
# max speed is max(df_deltas)/100
# for Condition, 1 = Avatar, 2 = Truck
# for Spread 1 = Randunif, 2 = Hard cutoff

#### Constants ####
travel_time <- 100

#### Libraries ####
library(tidyverse)
library(ggthemes)
library(psyphy)

#### Load in data ####
# Distribution info
load("scratch/data/df_beta")
# Click History
load("scratch/data/df_clickhist")
# Decision phase
load("scratch/data/df_decisions")
# Separations
load("scratch/data/df_deltas")
# Estimate phase
load("scratch/data/df_estimates")
# Screen information
load("scratch/data/df_screen_info")
# demo_phase 
load("scratch/data/df_demo_phase")
# avatar info 
load("scratch/data/df_avater_info")
# confidence 
load("scratch/data/df_confidence")

#### PLOTS: ####
#### PLOTS: Estimates vs actual ####
betas <- unique(df_decisions$Spread)

# setup empty frame to mirror estimates 
df_simulated <- data.frame(Participant = character(),
                           Spread = numeric(),
                           Delta = numeric(),
                           Estimate = numeric(),
                           Estimate_type = character())

# loop to get a simulation of what accuracy would look like 
# for each separation in each condition 
for(p in unique(df_estimates$Participant)){
  # subset 
  ss <- df_estimates[df_estimates$Participant == p,]
  
  # max speed 
  max_speed <- df_avatar_info$max_speed[df_avatar_info$Participant == p]
  
  # reach 
  reach <- df_avatar_info$reach[df_avatar_info$Participant == p]
  
  for(B in unique(betas)){
    Spread <- B
    
    # gen distances covered given defined distribution
    y <- (round(rbeta(100000, B, B)*max_speed)+1) * travel_time
    
    # make data_frame
    deltas <- data.frame(Participant = p,
                         Spread = B,
                         Delta = unique(ss$Delta))
    
    deltas <- deltas %>%
      group_by(Participant, Spread, Delta) %>%
      mutate(Estimate = sum(y >= Delta - reach)/length(y),
             Estimate_Type = "Simulated")
    
    # add to data frame
    df_simulated = rbind(df_simulated, as.data.frame(deltas))
    
  }
}

# tidy 
rm(deltas, ss, B, betas, max_speed, p, Spread, y, reach)

# add in truck_perf
df_simulated$truck_perf <- "Random_Uniform"
df_simulated$truck_perf[df_simulated$Spread > 1] = "Highly_Certain"

# bind data sets? 
df_est_sim <- rbind(df_estimates, df_simulated)

# tidy
rm(df_simulated)

# now get glm lines for this 
plt_estimates <- df_est_sim %>%
  mutate(Spread = as.factor(Spread)) %>%
  group_by(Participant, truck_perf, Delta, Estimate_Type) %>%
  summarise(Estimate = mean(Estimate)) %>%
  ggplot(aes(Delta, Estimate,
             colour = Estimate_Type,
             shape = Estimate_Type)) +
  geom_point() +
  geom_smooth(method = glm,
              method.args = list(family = "binomial"),
              aes(y = Estimate),
              se = F) +
  geom_smooth(data = df_demo_phase,
              method = glm,
              method.args = list(family = "binomial"),
              aes(y = Success),
              se = F) +
  theme_bw() +
  facet_wrap(~truck_perf + Participant) +
  theme(legend.position = "bottom",
        strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "mm")))
plt_estimates$labels$y <- "Estimated Accuracy"
plt_estimates$labels$colour <- "Estimate Type"
plt_estimates$labels$shape <- "Estimate Type"
plt_estimates

#### PLOTS: decision phase ####
plt_decisions <- df_decisions %>%
  mutate(Condition = as.factor(Condition),
         Spread = as.factor(Spread),
         Rand_first = as.factor(Rand_first),
         Norm_Placement = abs(Placed_x/Delta)) %>%
  ggplot(aes(Delta, Norm_Placement)) + #, colour = Rand_first)) + 
  geom_point(alpha = 0.2) + 
  theme_bw() + 
  facet_wrap(~truck_perf + Participant, ncol = 4) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "mm")))
plt_decisions$labels$x <- "Delta (pixels)"
plt_decisions$labels$y <- "Absolute Normalised Avatar Position"
plt_decisions

#### PLOTS: click history ####
# not sure what this should look like...


#### work in progress #### 

#### Opt Acc shaded regions ####
# sim acc for all possible dists 
# may have to do this by participant at some point if screen_res differs
# define this in the loop 
   
# setup dataframe 
df_success_rate <- data.frame(Delta = numeric(),
                              pos_dist = numeric(),
                              Spread = numeric(),
                              truck_perf = numeric(),
                              accuracy = numeric())

# loop through 
for(B in unique(df_beta$beta1)){
  # label truck_perf
  if(B > 2){
    truck_perf <- "Highly_Certain"
  } else {
    truck_perf <- "Random_Uniform"
  }
  
  # loop through Deltas
  for(D in unique(df_decisions$Delta)){
  # for(D in seq(200,max(df_decisions$Delta), 15)){
    # set parameters
    separations <- c(0:(2*D))
    max_speed <- unique(df_avatar_info$max_speed)
    reach <- unique(df_avatar_info$reach)
    
    # setup data frame 
    y <- (round(rbeta(100000, B, B)*max_speed)+1) * travel_time
    
    # make data_frame
    deltas <- data.frame(Delta = D,
                         pos_dist = unique(separations),
                         Spread = B,
                         truck_perf = truck_perf)
    
    deltas <- deltas %>%
      group_by(Spread, pos_dist) %>%
      mutate(Accuracy = sum(y >= (Delta - (Delta - pos_dist)) - reach)/length(y))
    
    df_success_rate <- rbind(df_success_rate, data.frame(deltas))
  }
}

# tidy 
rm(B, D, max_speed, reach, separations, truck_perf, y)


# sort out mapping for all possible locations 
temp <- df_success_rate %>%
  group_by(Delta, truck_perf, Spread) %>%
  mutate(left_Dist = pos_dist,
         left_Accuracy = Accuracy,
         right_Dist = max(pos_dist)-left_Dist)
temp2 <- df_success_rate %>%
  group_by(Delta, truck_perf, Spread) %>%
  mutate(right_Accuracy = Accuracy,
         right_Dist = pos_dist) %>%
  select(Spread,
         truck_perf,
         right_Accuracy,
         right_Dist)


df_pos_success <- merge(temp, temp2) %>%
  select(Delta,
         Spread,
         truck_perf,
         left_Dist,
         right_Dist,
         left_Accuracy,
         right_Accuracy) %>%
  mutate(Accuracy = 0.5*(right_Accuracy + left_Accuracy))
  

# tidy 
rm(temp, temp2)


# Do some range stuff with this data to simplify 
df_opt_acc <- df_pos_success %>%
  group_by(Delta, truck_perf, Accuracy, Spread) %>%
  mutate(left_Pos = left_Dist - median(left_Dist)) %>%
  ungroup() %>%
  mutate(Accuracy = round(Accuracy, digits = 2)) %>%
  group_by(Delta, truck_perf, Accuracy, Spread) %>%
  filter(left_Pos > -1) %>%
  summarise(lower_pos = min(left_Pos),
            upper_pos = max(left_Pos)) %>%
  ungroup() %>%
  group_by(Delta, truck_perf, Spread) %>%
  mutate(acceptable_acc = 0.92*(max(Accuracy)))


plt_opt_acc <- df_opt_acc %>%
  group_by(Delta, truck_perf, Spread) %>%
  filter(Accuracy > acceptable_acc) %>%
  mutate(min_pos = min(lower_pos),
         max_pos = max(upper_pos)) %>%
  ggplot(aes(Delta, upper_pos/Delta,
             fill = truck_perf)) + 
  geom_ribbon(aes(ymin = min_pos/Delta,
                  ymax = max_pos/Delta),
              alpha = 0.3) + 
  # line is more accurate, but has flat parts
  # geom_line(aes(Delta, Accuracy,
  #              colour = truck_perf)) +
  # smooth looks a bit nicer but doesn't reach floor soon enough
  geom_smooth(method = glm,
              method.args = list(family = binomial(mafc.logit(2))),
              aes(y = Accuracy,
                  colour = truck_perf),
              se = F) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Maximum Accuracy")) +
  theme_bw() + 
  scale_colour_ptol() + 
  scale_fill_ptol() +
  theme(legend.position = "bottom")
plt_opt_acc$labels$y <- "Normalised Absolute Position"
plt_opt_acc$labels$colour <- "Avatar Performance"
plt_opt_acc$labels$fill <- "Avatar Performance"
plt_opt_acc$labels$linetype <- "Avatar Performance"
plt_opt_acc

# Shaded regions correspond to the area in which the truck could be placed
# and still have a reasonably high chance of reaching the target. In this case,
# it corresponds to the area in which people could obtain 92% of the maximum 
# accuracy that could be achieved by the optimal strategy


#### Response times? ####
# overall plot of RT dist ~ truck_perf
df_decisions %>%
  mutate(log2_RT = log2(RT)) %>%
  ggplot(aes(log2_RT, fill = truck_perf)) + 
  geom_density(alpha = 0.2) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  scale_fill_ptol()

# same again by participant 
df_decisions %>%
  mutate(log2_RT = log2(RT)) %>%
  ggplot(aes(log2_RT, fill = truck_perf)) + 
  geom_density(alpha = 0.2) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  scale_fill_ptol() + 
  facet_wrap(~Rand_first + Participant)



# by separation + truck_perf
df_decisions %>% 
  mutate(log2_RT = log2(RT),
         separations = as.factor(Delta)) %>%
  ggplot(aes(log2_RT, fill = truck_perf)) + 
  geom_density(alpha = 0.2) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "mm"))) + 
  scale_fill_ptol() + 
  facet_wrap(~separations)


# just some raw numbers 
df_decisions %>% 
  group_by(Participant,
           Delta) %>% 
  summarise(mean_rt = mean(RT))


##### Confidence stuff ####
# How best to represent this
# this kind of works... might have to do some tweeking to make it 
# as clear as possible what's happened 
df_confidence %>% 
  # get some scaled versions of confidence to give 0 a meaning
  mutate(zeroed_conf = (Confidence - (max(df_screen_info$x_res/4))),
         scaled_conf = zeroed_conf/(max(df_screen_info$x_res)/2)) %>%
  group_by(Participant, Delta, Y_or_N, Spread) %>%
  summarise(avg_Confidence = mean(scaled_conf)) %>%
  ungroup() %>%
  # to convert confidence into estimates of accuracy
  mutate(est_acc = abs((Y_or_N - 1) + avg_Confidence),
         Spread = as.factor(Spread)) %>%
  ggplot(aes(Delta, est_acc, colour = Spread)) + 
  geom_jitter() + 
  facet_wrap(~Participant)

