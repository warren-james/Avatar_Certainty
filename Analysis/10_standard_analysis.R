#### 10_summary_script ####
# sort out some more straightforward analysis 
# for the AVATAR experiment

#### Library ####
library(betareg)
library(brms)
library(lme4)
library(rstan)
library(tidyverse)
library(tidybayes)

#### Constants ####

#### Functions ####

#### Load in Data ####
load("scratch/data/df_Aberdeen_decisions")
df_Aberdeen_decisions <- df_Aberdeen_decisions %>% 
  mutate(dist_type = ifelse(delta < median(delta), "close", "far"))

#### Mean and Variance in positioning ####
# for each participant
df_agg_p <- df_Aberdeen_decisions %>%
  group_by(participant, condition_label, truck_perf, delta, dist_type) %>% 
  mutate(abs_pos = abs(placed_x)/delta) %>%
  summarise(mean_pos = mean(abs_pos),
            var_pos = var(abs_pos))

# overall 
df_agg_o <- df_agg_p %>%
  group_by(condition_label, truck_perf, delta, dist_type) %>% 
  summarise(n = n(), 
            position = mean(mean_pos),
            variance = mean(var_pos),
            se_pos = sd(mean_pos)/sqrt(n),
            se_var = sd(var_pos)/sqrt(n))


#### Plots: Mean ####
# plt_mean_pos <- df_agg_p %>% 
#   ggplot(aes(delta, mean_pos,
#              colour = truck_perf,
#              fill = truck_perf)) + 
#   geom_path(aes(group = participant), alpha = 0.33) +
#   geom_smooth(se = FALSE, size = 3) + 
#   facet_wrap(~condition_label)
# above plot is ugly...

# plot using overall data 
plt_mean_pos <- df_agg_o %>%
  ggplot(aes(delta, position,
             colour = condition_label,
             fill = condition_label,
             ymin = position - se_pos,
             ymax = position + se_pos)) + 
  geom_line() + 
  geom_errorbar() + 
  theme_minimal() + 
  facet_wrap(~truck_perf)
plt_mean_pos$labels$x <- "Delta (pixels)"
plt_mean_pos$labels$y <- "Normalised Placement"
plt_mean_pos$labsls$colour <- "Condition"
plt_mean_pos$labsls$fill <- "Condition"
plt_mean_pos

plt_mean_pos_2 <- df_agg_o %>%
  ggplot(aes(delta, position,
             colour = truck_perf,
             fill = truck_perf,
             ymin = position - se_pos,
             ymax = position + se_pos)) + 
  geom_line() + 
  geom_errorbar() + 
  theme_minimal() + 
  facet_wrap(~condition_label)
plt_mean_pos_2$labels$x <- "Delta (pixels)"
plt_mean_pos_2$labels$y <- "Normalised Placement"
plt_mean_pos_2$labsls$colour <- "Truck Performance"
plt_mean_pos_2$labsls$fill <- "Truck Performance"
plt_mean_pos_2

#### Plots: Variance ####
plt_var_pos <- df_agg_o %>% 
  ggplot(aes(delta, variance,
             colour = truck_perf,
             fill = truck_perf,
             ymin = variance - se_var,
             ymax = variance + se_var)) + 
  geom_line() + 
  geom_errorbar() + 
  theme_minimal() + 
  facet_wrap(~condition_label)
plt_var_pos$labels$x <- "Delta (pixels)"
plt_var_pos$labels$y <- "Variance in  Placement"
plt_var_pos$labels$colour <- "Truck Performance"
plt_var_pos$labels$fill <- "Truck Performance"
plt_var_pos

plt_var_pos_2 <- df_agg_o %>% 
  ggplot(aes(delta, variance,
             colour = condition_label,
             fill = condition_label,
             ymin = variance - se_var,
             ymax = variance + se_var)) + 
  geom_line() + 
  geom_errorbar() + 
  theme_minimal() + 
  facet_wrap(~truck_perf)
plt_var_pos_2$labels$x <- "Delta (pixels)"
plt_var_pos_2$labels$y <- "Variance in  Placement"
plt_var_pos_2$labels$colour <- "Condition"
plt_var_pos_2$labels$fill <- "Condition"
plt_var_pos_2

#### Plots: distribution plots ####
# use all data and dist_type 
plt_dist_all <- df_Aberdeen_decisions %>% 
  mutate(abs_pos = abs(placed_x)/delta) %>%
  filter(abs_pos <= 1) %>%
  ggplot(aes(abs_pos,
             colour = truck_perf,
             fill = truck_perf)) + 
  geom_density(alpha = 0.3) + 
  facet_grid(condition_label~dist_type) + 
  theme_minimal() + 
  ggthemes::scale_color_ptol() + 
  ggthemes::scale_fill_ptol()
plt_dist_all


# same as above using mean data 
plt_dist_mean_p <- df_agg_p %>%
  group_by(participant, condition_label, truck_perf, dist_type) %>%
  summarise(mean_pos = mean(mean_pos)) %>%
  ggplot(aes(mean_pos, 
             colour = truck_perf,
             fill = truck_perf)) + 
  geom_density(alpha = 0.3) + 
  facet_grid(condition_label~dist_type) + 
  theme_minimal() + 
  ggthemes::scale_color_ptol() + 
  ggthemes::scale_fill_ptol()
plt_dist_mean_p


#### Analysis ####
#### Analysis: ANOVAs ####
# problem here is that the data is not "normal"
# so take this with a pinch of salt
aov_pos <- aov(data = df_agg_o, position ~ truck_perf * delta * condition_label)
aov_var <- aov(data = df_agg_o, variance ~ delta * truck_perf * condition_label)

#### Analysis: glms? ####
# glm1 <- glmer(position ~ (truck_perf + delta + condition_label)^3,
#               data = df_agg_o,
#               family = )

betareg_1 <- betareg(position ~ (truck_perf + delta + condition_label)^3,
                     data = df_agg_o)

betareg_2 <- betareg(mean_pos ~ (truck_perf + delta + condition_label)^3,
                     data = model_data)


#### Setup SPSS files ####
SPSS_mean <- df_agg_p %>%
  select(-var_pos) %>%
  mutate(delta = round(delta)) %>%
  unite("value",
        truck_perf:delta) %>%
  spread(value, mean_pos)

# save file
write.csv(SPSS_mean, file = "scratch/data/SPSS_mean_Aberdeen.txt", row.names = F)
