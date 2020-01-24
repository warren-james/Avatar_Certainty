#### Avatar - Make some plots ####
# plots of decisions 
# compare estimates to actual performance curves
# Something with number of clicks?
# RT may only be useful for modelling?

#### Notes ####
# max speed is max(df_deltas)/100
# for Condition, 1 = Avatar, 2 = Truck
# for Spread 1 = Randunif, 2 = Hard cutoff

#### Any functions ####
prob_success <- function(delta, beta, max_speed, opt){
  # get observations
  y <- (round(rbeta(100000, beta, beta)*max_speed)+1) * travel_time + 30
  
  # get prob of success
  acc <- sum(y > delta)/length(y)
  
  # check if opt strat or not
  if(opt == TRUE && acc < 0.5){
      acc <- 0.5
  }
  
  # output 
  return(round(acc, digits = 3))
}

#### Constants ####
travel_time <- 100

#### Libraries ####
library(tidyverse)
library(ggthemes)
library(psyphy)
library(brms)

#### Load in data ####
# Distribution info
load("scratch/data/df_Aberdeen_beta")
# Click History
load("scratch/data/df_Aberdeen_clickhist")
# Decision phase
load("scratch/data/df_Aberdeen_decisions")
# Separations
load("scratch/data/df_Aberdeen_deltas")
# Estimate phase
load("scratch/data/df_Aberdeen_estimates")
# Screen information
load("scratch/data/df_Aberdeen_screen_info")
# demo_phase 
load("scratch/data/df_Aberdeen_demo_phase")
# avatar info 
load("scratch/data/df_Aberdeen_avater_info")
# confidence 
load("scratch/data/df_Aberdeen_confidence")

#### process ####
# 3 participants had less trials for some reason 
# not sure why 
# let's remove them 
remove_particiapnts <- c("40", "46", "63")

#### PLOTS: ####
#### PLOTS: Estimates vs actual ####
betas <- unique(df_Aberdeen_decisions$spread)

# setup empty frame to mirror estimates 
df_Aberdeen_simulated <- data.frame(participant = character(),
                           spread = numeric(),
                           delta = numeric(),
                           estimate = numeric(),
                           estimate_type = character())


# loop to get a simulation of what accuracy would look like 
# for each separation in each condition 
for(p in unique(df_Aberdeen_estimates$participant)){
  # subset 
  ss <- df_Aberdeen_estimates[df_Aberdeen_estimates$participant == p,]
  
  # max speed 
  max_speed <- df_Aberdeen_avatar_info$max_speed[df_Aberdeen_avatar_info$participant == p]
  
  # reach 
  reach <- df_Aberdeen_avatar_info$reach[df_Aberdeen_avatar_info$participant == p]
  
  for(B in unique(betas)){
    Spread <- B
    
    # gen distances covered given defined distribution
    y <- (round(rbeta(100000, B, B)*max_speed)+1) * travel_time
    
    # make data_frame
    # just unique(ss$delta)
    deltas <- data.frame(participant = p,
                         spread = B,
                         delta = unique(ss$delta))
    
    deltas <- deltas %>%
      group_by(participant, spread, delta) %>%
      mutate(estimate = sum(y >= delta - reach)/length(y),
             estimate_type = "Simulated")
    
    # add to data frame
    df_Aberdeen_simulated = rbind(df_Aberdeen_simulated, as.data.frame(deltas))
  }
}

# tidy 
rm(deltas, ss, B, betas, max_speed, p, Spread, y, reach)

# add in truck_perf
df_Aberdeen_simulated$truck_perf <- "Variable"
df_Aberdeen_simulated$truck_perf[df_Aberdeen_simulated$spread > 1] = "Constant"

# bind data sets? 
df_Aberdeen_est_sim <- rbind(df_Aberdeen_estimates, df_Aberdeen_simulated)

# now get glm lines for this 
plt_estimates <- df_Aberdeen_est_sim %>%
  mutate(Spread = as.factor(spread)) %>%
  group_by(participant, truck_perf, delta, estimate_type) %>%
  summarise(estimate = mean(estimate)) %>%
  ggplot(aes(delta, estimate,
             colour = estimate_type,
             shape = estimate_type)) +
  geom_point() +
  geom_smooth(method = glm,
              method.args = list(family = "binomial"),
              aes(y = estimate),
              se = F) +
  # geom_smooth(data = df_demo_phase,
  #             method = glm,
  #             method.args = list(family = "binomial"),
  #             aes(y = success),
  #             se = F) +
  theme_bw() +
  scale_colour_ptol() + 
  facet_wrap(~truck_perf + participant, ncol = 10) +
  theme(legend.position = "bottom",
        strip.text.x = element_blank())
plt_estimates$labels$y <- "Estimated Accuracy"
plt_estimates$labels$colour <- "Estimate Type"
plt_estimates$labels$shape <- "Estimate Type"
plt_estimates

# save 
ggsave("scratch/plots/plt_estimates.png",
       height = 17,
       width = 24,
       units = "cm")


#### PLOTS: decision phase ####
plt_decisions <- df_Aberdeen_decisions %>%
  mutate(Condition = as.factor(condition),
         Spread = as.factor(spread),
         Rand_first = as.factor(rand_first),
         Norm_Placement = abs(placed_x/delta)) %>%
  filter(Norm_Placement < 1.01) %>%
  ggplot(aes(delta, Norm_Placement, 
             colour = truck_perf)) + 
  geom_point(alpha = 0.2) + 
  theme_bw() + 
  facet_wrap(~truck_perf + participant, ncol = 10) + 
  theme(legend.position = "bottom",
        strip.text.x = element_blank()) + 
  scale_colour_ptol()
plt_decisions$labels$x <- "Delta (pixels)"
plt_decisions$labels$y <- "Absolute Normalised Avatar Position"
plt_decisions

# save 
ggsave("scratch/plots/plt_decisions.png",
       height = 17,
       width = 24,
       units = "cm")

#### PLOT: decisions phase again... with order info ####
plt_decisions <- df_Aberdeen_decisions %>%
  filter(!participant %in% remove_particiapnts) %>% 
  mutate(Condition = as.factor(condition),
         Spread = as.factor(spread),
         Rand_first = as.factor(rand_first),
         Norm_Placement = abs(placed_x/delta)) %>%
  filter(Norm_Placement < 1.01) %>%
  ggplot(aes(delta, Norm_Placement, 
             colour = truck_perf)) + 
  geom_point(alpha = 0.2) + 
  theme_bw() + 
  facet_wrap(~participant + block + truck_perf, ncol = 10) + 
  theme(legend.position = "bottom",
        strip.text.x = element_blank()) + 
  scale_colour_ptol()
plt_decisions$labels$x <- "Delta (pixels)"
plt_decisions$labels$y <- "Absolute Normalised Avatar Position"
plt_decisions

# save 
ggsave("scratch/plots/plt_decisions_order.png",
       height = 17,
       width = 24,
       units = "cm")

#### PLOT: chance of success on y-axis ####
#### Fix at some point... doesn't work... 
plt_acc <- df_Aberdeen_decisions %>%
  #mutate(Participant = as.factor(participant)) %>%
  group_by(participant, delta, truck_perf, spread) %>%
  summarise(mean_acc = mean(chance)) %>%
  ungroup() %>%
  ggplot(aes(delta, mean_acc,
             colour = truck_perf)) + 
  geom_path(aes(group = interaction(participant, truck_perf)),
            alpha = 0.2,
            size = 1) + 
  scale_colour_ptol() + 
  theme_bw()  
plt_acc

#### PLOT: add in strat lines #### 
# TODO Fix this
# first add max speed to the data 
max_speed <- df_Aberdeen_avatar_info %>% 
  select(-reach)
# get success rates
plt_acc_2 <- merge(plt_acc$data, max_speed) %>%
  rowwise() %>% 
  mutate(opt_acc = prob_success(delta, spread, max_speed, TRUE),
         cen_acc = prob_success(delta, spread, max_speed, FALSE)) %>%
  ungroup() %>%
  gather(opt_acc:cen_acc, 
         key = "strategy",
         value = "accuracy") %>%
  group_by(delta, truck_perf, strategy) %>%
  summarise(mean_acc = mean(accuracy)) %>%
  ungroup()
  

plt_acc <- plt_acc +
  geom_line(data = plt_acc_2,
            aes(delta, mean_acc,
                group = interaction(truck_perf, strategy),
                linetype = strategy,
                colour = truck_perf),
            size = 1) + 
  scale_linetype_manual(values = c("twodash", "longdash")) + 
  facet_wrap(~truck_perf) + 
  theme(strip.text.x = element_blank())
plt_acc$labels$x <- "Delta (pixels)"
plt_acc$labels$y <- "Average Accuracy"
plt_acc$labels$colour <- "Condition"
plt_acc$labels$linetype <- "Strategy"
plt_acc

# save


#### PLOT: Ribbon plot ####
# setup data
df_Aberdeen_decisions %>% 
   select(participant, delta, truck_perf, chance) %>%
   group_by(participant, delta, truck_perf) %>%
   summarize(
     exp_chance = mean(chance)) %>%
   group_by(delta, truck_perf) %>%
   summarize(
     y1_min = quantile(exp_chance, 0),
     y1_max = quantile(exp_chance, 1),
     
     y2_min = quantile(exp_chance, 0.1),
     y2_max = quantile(exp_chance, 0.9),
     
     y3_min = quantile(exp_chance, 0.2),
     y3_max = quantile(exp_chance, 0.8),
     
     y4_min = quantile(exp_chance, 0.3),
     y4_max = quantile(exp_chance, 0.7),
     
     y5_min = quantile(exp_chance, 0.4),
     y5_max = quantile(exp_chance, 0.6)
   ) %>%
   gather(y1_min:y5_max, key = ribbon, value = y) %>%
   separate(ribbon, into = c("rib_band", "minmax")) %>%
   spread(minmax, y)  -> df_Aberdeen_ribbon

plt <- ggplot()
plt <- plt + geom_line(data = plt_acc_2,
                       aes(x = delta, y = mean_acc,
                           group = interaction(truck_perf, strategy),
                           linetype = strategy,
                           colour = truck_perf),
                       size = 1)
plt <- plt + scale_linetype_manual(values = c("twodash", "longdash"))
plt <- plt + geom_ribbon(data = df_Aberdeen_ribbon,
                         aes(x = delta, 
                             ymin = min, 
                             ymax = max,
                             group = interaction(rib_band, truck_perf),
                             fill = truck_perf),
                         alpha = 0.3)
plt <- plt + scale_colour_manual(values = c("#CCDDAA","#BBCCEE"))
plt <- plt + scale_fill_manual(values = c("#CCDDAA","#BBCCEE"))
plt <- plt + facet_wrap(~truck_perf)
plt <- plt + theme_bw()
plt <- plt + theme(strip.text.x = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())
plt



#### PLOT: placement by furthest and closest, and Condition ####
# plot of placement position
df_Aberdeen_decisions$Norm_Delta <- df_Aberdeen_decisions$delta/max(df_Aberdeen_decisions$delta)

# sort out dist_type column
df_Aberdeen_decisions$dist_type <- "close"
df_Aberdeen_decisions$dist_type[df_Aberdeen_decisions$Norm_Delta > 0.5] <- "far"

# Make the plot
plt_dist_dec <- df_Aberdeen_decisions %>% 
  mutate(abs_pos = abs(placed_x)/delta) %>%
  group_by(participant, dist_type, truck_perf, condition_label) %>%
  summarise(pos = mean(abs_pos)) %>%
  ungroup() %>%
  ggplot(aes(pos, 
             colour = truck_perf,
             fill = truck_perf)) + 
  geom_density(alpha = 0.3) + 
  facet_grid(condition_label~dist_type)
plt_dist_dec$labels$x <- "Normalised Placement"
plt_dist_dec$labels$colour <- "Condition"
plt_dist_dec$labels$fill <- "Condition"
plt_dist_dec

# save 
ggsave("scratch/plots/plt_dist_dec.png",
       height = 8,
       width = 14,
       units = "cm")

# same again but without getting means 
plt_dist_dec_2 <- df_Aberdeen_decisions %>%
  group_by(participant) %>%
  mutate(mid_delta = mean(as.numeric(delta))) %>%
  filter(as.numeric(delta) != mid_delta)
# add new labels
plt_dist_dec_2$dist_type <- "Close"
plt_dist_dec_2$dist_type[as.numeric(plt_dist_dec_2$delta) > plt_dist_dec_2$mid_delta] <- "Far"
# make plot 
plt_dist_dec_2 <- plt_dist_dec_2 %>%
  mutate(abs_pos = abs(placed_x)/delta) %>%
  filter(abs_pos < 1.01) %>%
  ggplot(aes(abs_pos, 
             colour = truck_perf,
             fill = truck_perf)) + 
  geom_density(alpha = 0.3) +
  # facet_wrap(~dist_type) + 
  facet_grid(condition_label~dist_type) + 
  scale_colour_ptol() + 
  scale_fill_ptol() + 
  theme_bw()
plt_dist_dec_2
  
# save 
ggsave("scratch/plots/plt_dist_dec_2.png",
       height = 8,
       width = 14,
       units = "cm")

# without split, just delta 
plt_dist_dec_2$data %>%
  ggplot(aes(abs_pos, 
             colour = truck_perf,
             fill = truck_perf)) + 
  geom_density(alpha = 0.3) + 
  facet_wrap(~delta)

# setup model data 
model_data <- plt_dist_dec_2[["data"]] %>% 
  filter(abs_pos <= 0.9999,
         abs_pos >= 0.00001)

# plot quick 
model_data %>% 
  ggplot(aes(abs_pos, 
             colour = truck_perf,
             fill = truck_perf)) + 
  geom_density(alpha = 0.3) + 
  facet_grid(condition_label~dist_type)

# quick model
m4 <- brm(abs_pos ~ (condition_label + dist_type + truck_perf)^2, 
          data = model_data, 
          family = "beta",
          iter = 2000,
          chains = 1, 
          cores = 1)

# save for now... looks promising anyway
save(m4, file = "models/outputs/brms_m4")

# add rand intercepts?
m4.1 <- brm(abs_pos ~ (dist_type + truck_perf)^2 + (1|participant), 
            data = model_data, 
            family = "beta",
            iter = 2000,
            chains = 1, 
            cores = 1)

# save for now 
save(m4.1, file = "models/outputs/brms_m4.1")

#### work in progress #### 
# some things that might not be needed... but might be
# so I don't want to delete them 

#### PLOTS: click history ####
# not sure what this should look like...

# maybe num clicks by condition and distance?
df_Aberdeen_clickhist %>%
  mutate(Spread = as.factor(Spread)) %>%
  group_by(Trial, Delta, Participant, Spread) %>%
  summarise(max_clicks = max(click_num)) %>%
  ungroup() %>%
  ggplot(aes(max_clicks,
             colour = Spread,
             fill = Spread)) + 
  geom_histogram(position = "dodge") + 
  facet_wrap(~Delta)

#### Opt Acc shaded regions ####
# sim acc for all possible dists 
# may have to do this by participant at some point if screen_res differs
# define this in the loop 
   
# setup dataframe 
df_Aberdeen_success_rate <- data.frame(Delta = numeric(),
                              pos_dist = numeric(),
                              Spread = numeric(),
                              truck_perf = numeric(),
                              accuracy = numeric())

# loop through 
for(B in unique(df_Aberdeen_beta$beta1)){
  # label truck_perf
  if(B > 2){
    truck_perf <- "Highly_Certain"
  } else {
    truck_perf <- "Random_Uniform"
  }
  
  # loop through Deltas
  for(D in unique(df_Aberdeen_decisions$Delta)){
  # for(D in seq(200,max(df_Aberdeen_decisions$Delta), 15)){
    # set parameters
    separations <- c(0:(2*D))
    max_speed <- unique(df_Aberdeen_avatar_info$max_speed)
    reach <- unique(df_Aberdeen_avatar_info$reach)
    
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
    
    df_Aberdeen_success_rate <- rbind(df_Aberdeen_success_rate, data.frame(deltas))
  }
}

# tidy 
rm(B, D, max_speed, reach, separations, truck_perf, y)


# sort out mapping for all possible locations 
temp <- df_Aberdeen_success_rate %>%
  group_by(Delta, truck_perf, Spread) %>%
  mutate(left_Dist = pos_dist,
         left_Accuracy = Accuracy,
         right_Dist = max(pos_dist)-left_Dist)
temp2 <- df_Aberdeen_success_rate %>%
  group_by(Delta, truck_perf, Spread) %>%
  mutate(right_Accuracy = Accuracy,
         right_Dist = pos_dist) %>%
  select(Spread,
         truck_perf,
         right_Accuracy,
         right_Dist)


df_Aberdeen_pos_success <- merge(temp, temp2) %>%
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
df_Aberdeen_opt_acc <- df_Aberdeen_pos_success %>%
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


plt_opt_acc <- df_Aberdeen_opt_acc %>%
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
df_Aberdeen_decisions %>%
  mutate(log2_RT = log2(RT)) %>%
  ggplot(aes(log2_RT, fill = truck_perf)) + 
  geom_density(alpha = 0.2) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  scale_fill_ptol() +
  facet_wrap(~Rand_first)

# same again by participant 
df_Aberdeen_decisions %>%
  mutate(log2_RT = log2(RT)) %>%
  ggplot(aes(log2_RT, fill = truck_perf)) + 
  geom_density(alpha = 0.2) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        strip.text.x = element_blank()) + 
  scale_fill_ptol() + 
  facet_wrap(~Rand_first + Participant)



# by separation + truck_perf
df_Aberdeen_decisions %>% 
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
df_Aberdeen_decisions %>% 
  group_by(Participant,
           Delta) %>% 
  summarise(mean_rt = mean(RT))


##### Confidence stuff ####
# How best to represent this
# this kind of works... might have to do some tweeking to make it 
# as clear as possible what's happened 
# df_confidence %>% 
#   # get some scaled versions of confidence to give 0 a meaning
#   mutate(zeroed_conf = (Confidence - (max(df_screen_info$x_res/4))),
#          scaled_conf = zeroed_conf/(max(df_screen_info$x_res)/2)) %>%
#   group_by(Participant, Delta, Y_or_N, Spread) %>%
#   summarise(avg_Confidence = mean(scaled_conf)) %>%
#   ungroup() %>%
#   # to convert confidence into estimates of accuracy
#   mutate(est_acc = abs((Y_or_N - 1) + avg_Confidence),
#          Spread = as.factor(Spread)) %>%
#   ggplot(aes(Delta, est_acc, colour = Spread)) + 
#   geom_jitter() + 
#   facet_wrap(~Participant)


