#### Certainty ####
# Looking at how participants used the certainty measuere 
# and how they used the x out of 10 measure 

#### Library ####
library(tidyverse)

#### read in data ####
load("scratch/data/df_Aberdeen_confidence")
load("scratch/data/df_Aberdeen_screen_info")

#### CONFIDENCE ####
#### pre-processing ####
df_Aberdeen_confidence <- merge(df_Aberdeen_screen_info, df_Aberdeen_confidence) %>% 
  mutate(max_x = x_res/2,
         confidence_val = confidence/max_x,
         truck_perf = ifelse(spread > 1, "Constant", "Variable")) %>% 
  select(participant, truck_perf, delta, y_or_n, confidence_val) %>% 
  group_by(participant) %>% 
  mutate(delta_num = as.numeric(as.factor(delta)))

#### Plotting ####
df_Aberdeen_confidence %>% 
  mutate(delta_num = as.factor(delta_num)) %>%
  ungroup() %>% 
  group_by(participant, delta_num, truck_perf) %>% 
  summarise(confidence_val = mean(confidence_val)) %>%
  ggplot(aes(delta_num, confidence_val,
             colour = truck_perf,
             fill = truck_perf)) + 
  geom_boxplot(position = "dodge",
               alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat()

# try some lines? 
df_Aberdeen_confidence %>% 
  ungroup() %>% 
  group_by(participant, delta_num, truck_perf) %>% 
  summarise(confidence_val = mean(confidence_val),
            yes = mean(y_or_n)) %>%
  ggplot(aes(delta_num, confidence_val,
             colour = truck_perf)) + 
  #geom_line(aes(group = participant)) + 
  geom_line() +
  geom_point(aes(y = yes),
             alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  facet_wrap(~participant) + 
  theme(strip.text.x = element_blank())


#### OUT OF 10 ####
load("scratch/data/df_Aberdeen_estimates")

#### pre-processing ####
df_Aberdeen_estimates <- df_Aberdeen_estimates %>% 
  group_by(participant) %>% 
  mutate(delta_num = as.numeric(as.factor(delta))) %>% 
  select(participant, truck_perf,
         delta_num, estimate) %>% 
  ungroup()

#### Plotting ####
df_Aberdeen_estimates %>%
  group_by(participant, delta_num, truck_perf) %>% 
  summarise(estimate = mean(estimate)) %>% 
  ungroup() %>% 
  mutate(delta_num = as.factor(delta_num)) %>%
  ggplot(aes(delta_num, estimate, 
             colour = truck_perf,
             fill = truck_perf)) + 
  geom_boxplot(alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat()

df_Aberdeen_estimates %>% 
  group_by(participant, delta_num, truck_perf) %>% 
  summarise(estimate = mean(estimate)) %>% 
  ggplot(aes(delta_num, estimate,
             colour = truck_perf)) + 
  geom_line() + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  theme(strip.text.x = element_blank()) + 
  facet_wrap(~participant)
