library(tidyverse)

load("scratch/data/df_decisions")

df_decisions <- as.tibble(df_decisions)


# compute mean and var for each person and condition
(df_decisions %>% 
	select(Participant, truck_perf, Delta, Placed_x) %>%
	mutate(norm_dist = abs(Placed_x / Delta)) %>%
	group_by(Participant, truck_perf, Delta) %>%
	summarise(
		mean_position = mean(norm_dist),
		var_dist = var(norm_dist))) -> df

plt <- ggplot(df, aes(x = Delta, y = mean_position, colour = truck_perf))
plt <- plt + geom_path(aes(group = Participant), alpha = 0.33)
plt <- plt + geom_smooth(se = FALSE, size = 3)
plt <- plt + scale_colour_viridis_d(end = 0.5)
plt 

# now aggregate to compute mean of means and related standard error

(df %>% 
	group_by(truck_perf, Delta) %>%
	summarise(
		n = n(),
		position = mean(mean_position),
		std_err_p = sd(mean_position)/ sqrt(n),
		variance = mean(var_dist),
		std_err_v = sd(var_dist)/ sqrt(n))) -> df2

# plot how mean position varies with delta and condition 
plt <- ggplot(df2, aes(
	x = Delta, 
	y = position, 
	ymin = position - 1.96 * std_err_p,
	ymax = position + 1.96 * std_err_p,
	colour = truck_perf))
plt <- plt + geom_errorbar(colour = "gray") + geom_path()
plt <- plt + scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
plt <- plt + scale_x_continuous(breaks = unique(df$Delta))
plt <- plt + theme_bw()
plt

# plot how variance position varies with delta and condition 
plt <- ggplot(df2, aes(
	x = Delta, 
	y = variance, 
	ymin = variance - 1.96 * std_err_v,
	ymax = variance + 1.96 * std_err_v,
	colour = truck_perf))
plt <- plt + geom_errorbar(colour = "gray") + geom_path()
plt <- plt + scale_y_continuous(expand = c(0, 0))
plt <- plt + scale_x_continuous(breaks = unique(df$Delta))
plt <- plt + theme_bw()
plt

