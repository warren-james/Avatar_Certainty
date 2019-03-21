library(tidyverse)
library(ggthemes)

load("scratch/data/df_Essex_decisions")

df_decisions <- as.tibble(df_Essex_decisions)


# compute mean and var for each person and condition
(df_decisions %>% 
	select(participant, truck_perf, delta, placed_x) %>%
	mutate(norm_dist = abs(placed_x / delta)) %>%
	group_by(participant, truck_perf, delta) %>%
	summarise(
		mean_position = mean(norm_dist),
		var_dist = var(norm_dist))) -> df

plt <- ggplot(df, aes(x = delta, y = mean_position, colour = truck_perf))
plt <- plt + geom_path(aes(group = participant), alpha = 0.33)
plt <- plt + geom_smooth(se = FALSE, size = 3)
plt <- plt + scale_colour_viridis_d(end = 0.5)
plt 

# now aggregate to compute mean of means and related standard error

(df %>% 
	group_by(truck_perf, delta) %>%
	summarise(
		n = n(),
		position = mean(mean_position),
		std_err_p = sd(mean_position)/ sqrt(n),
		variance = mean(var_dist),
		std_err_v = sd(var_dist)/ sqrt(n))) -> df2

position_aov <- aov(data = df2, position ~ truck_perf * delta)
summary(position_aov)

variance_aov <- aov(data = df2, position ~ truck_perf * delta)
summary(variance_aov)


# plot how mean position varies with delta and condition 
plt_mean <- ggplot(df2, aes(
	x = delta, 
	y = position, 
	ymin = position - 1.96 * std_err_p,
	ymax = position + 1.96 * std_err_p,
	colour = truck_perf))
plt_mean <- plt_mean + geom_errorbar(colour = "gray") + geom_path()
plt_mean <- plt_mean + scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
plt_mean <- plt_mean + scale_x_continuous(breaks = unique(df$delta))
plt_mean <- plt_mean + theme_bw()
plt_mean

# plot how variance position varies with delta and condition 
plt <- ggplot(df2, aes(
	x = delta, 
	y = variance, 
	ymin = variance - 1.96 * std_err_v,
	ymax = variance + 1.96 * std_err_v,
	colour = truck_perf))
plt <- plt + geom_errorbar(colour = "gray") + geom_path()
plt <- plt + scale_y_continuous(expand = c(0, 0))
plt <- plt + scale_x_continuous(breaks = unique(df$delta))
plt <- plt + scale_colour_ptol()
plt <- plt + theme_bw()
plt$labels$x <- "Delta (in pixels)"
plt$labels$y <- "Variance"
plt$labels$colour <- "Performance Condition"
plt

# anovas 
pos_aov <- aov(data = df2, position ~ truck_perf * delta)
var_aov <- aov(data = df2, variance ~ delta * truck_perf)

#### SPSS data ####
# sort mean pos 
SPSS_mean <- df %>%
  group_by(participant) %>%
  mutate(mean_position = round(mean_position, digits = 3)) %>%
  select(-var_dist) %>%
  unite("value",
        truck_perf:delta) %>%
  spread(value, mean_position)

SPSS_var <- df %>%
  group_by(participant) %>%
  select(-mean_position) %>%
  unite("value",
        truck_perf:delta) %>%
  spread(value, var_dist)

  
# save file
write.csv(SPSS_mean, file = "scratch/data/SPSS_mean.txt", row.names = F)
write.csv(SPSS_var, file = "scratch/data/SPSS_var.txt", row.names = F)


