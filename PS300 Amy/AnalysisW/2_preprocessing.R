#### Processing data ####
# sort out data frames to be useful 

#### library ####
library(tidyverse) 

#### Functions ####
# squash range to be suitable for beta analysis
squash <- function(y, max, min, squash){
  y <- y * ((max-squash) - (min + squash)) + (min + squash)
  return(y)
}

#### load in data ####
load("scratch/df_decisions")
load("scratch/df_clickhist")
load("scratch/df_screen_info")
load("scratch/df_avatar_info")
load("scratch/df_demographics")

#### demographic information ####
df_demographics %>% 
  summarise(mu_age = mean(age),
            sd_age = sd(age),
            med_age = median(age),
            upper = max(age),
            lower = min(age))
df_demographics %>% 
  group_by(gender) %>% 
  summarise(n = n())

#### decision data ####
df_decisions <- df_decisions %>% 
  mutate(norm_placement = abs(Placed_x)/Delta,
         truck_perf = ifelse(Spread == 1, "Random",
                             ifelse(Spread == 10, "Uncertain", "Certain")),
         truck_perf = factor(truck_perf, levels = c("Random", "Uncertain", "Certain")),
         dist_type = ifelse(Delta < max(Delta)/2, "Close", "Far"))

# sort out new files
df_agg <- df_decisions %>% 
  group_by(Participant, truck_perf, dist_type) %>% 
  summarise(mean_placement = round(mean(norm_placement), digits = 3))
df_wide_agg <- df_agg %>% 
  mutate(dist_cond = paste(dist_type, truck_perf, sep = "_")) %>%
  ungroup() %>%
  select(Participant, dist_cond, mean_placement) %>%
  spread(key = dist_cond, 
         value = mean_placement)

# save these files
write.csv(df_agg, file = "scratch/new_data_files/df_agg.txt", row.names = F)
write.csv(df_wide_agg, file = "scratch/new_data_files/df_agg_wide.txt", row.names = F)

# plot 
df_decisions %>% 
  ggplot(aes(norm_placement, colour = truck_perf, fill = truck_perf)) + 
  geom_density(alpha = .3) + 
  facet_wrap(~Delta) + 
  theme_bw() + 
  see::scale_color_flat() + 
  see::scale_fill_flat()

# try mean version 
df_decisions %>% 
  group_by(Participant, truck_perf, dist_type) %>% 
  summarise(mu_pos = mean(norm_placement)) %>%
  ggplot(aes(mu_pos, colour = truck_perf, fill = truck_perf)) + 
  geom_density(alpha = .3) + 
  # geom_histogram(aes(y = ..density..),
  #                position = "dodge",
  #                alpha = .3) + 
  facet_wrap(~dist_type) + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(0,1,.25), 
                     limits = c(0,1)) + 
  see::scale_color_flat() + 
  see::scale_fill_flat()

# box plot version 
df_agg %>%
  ggplot(aes(dist_type, mean_placement,
             colour = truck_perf, 
             fill = truck_perf)) + 
  geom_boxplot(alpha = .3) + 
  geom_point(position = position_jitterdodge(.1)) +
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  theme_bw() +
  scale_y_continuous("Mean Normalised Placement",
                     # breaks = seq(0,1,.25),
                     breaks = c(0,1),
                     labels = c("Centre", "Side"),
                     limits = c(0,1)) + 
  scale_x_discrete("Distance Type") + 
  guides(fill = guide_legend(title = "Condition"),
         colour = guide_legend(title = "Condition"))
  
  

#### run a model? ####
library(brms)
model_data <- df_decisions %>% 
  filter(norm_placement <= 1) %>% 
  ungroup() %>%
  mutate(norm_placement = squash(norm_placement, 1, 0, 1e-5))
  
m1 <- brm(norm_placement ~ (dist_type + truck_perf)^2,
          data = model_data,
          family = "beta",
          chains = 1,
          iter = 2000,
          warmup = 1000)





