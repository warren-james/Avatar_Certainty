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

#### Mean and Variance in positioning ####
# for each participant
df_agg_p <- df_Aberdeen_decisions %>%
  group_by(participant, condition_label, truck_perf, delta) %>% 
  mutate(abs_pos = abs(placed_x)/delta) %>%
  summarise(mean_pos = mean(abs_pos),
            var_pos = var(abs_pos))

# overall 
df_agg_o <- df_agg_p %>%
  group_by(condition_label, truck_perf, delta) %>% 
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
             colour = truck_perf,
             fill = truck_perf,
             ymin = position - se_pos,
             ymax = position + se_pos)) + 
  geom_line() + 
  geom_errorbar() + 
  theme_minimal() + 
  facet_wrap(~condition_label)
plt_mean_pos$labels$x <- "Delta (pixels)"
plt_mean_pos$labels$y <- "Normalised Placement"
plt_mean_pos$labsls$colour <- "Truck Performance"
plt_mean_pos$labsls$fill <- "Truck Performance"
plt_mean_pos

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
plt_var_pos$labsls$colour <- "Truck Performance"
plt_var_pos$labsls$fill <- "Truck Performance"
plt_var_pos

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
