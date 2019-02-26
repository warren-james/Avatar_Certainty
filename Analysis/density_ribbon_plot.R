library(tidyverse)


load("scratch/data/df_decisions")

(df_decisions %>% 
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
	spread(minmax, y))  -> df_ribbon

plt <- ggplot(df_ribbon, aes(x = delta, ymin = min, ymax = max, fill = truck_perf))
plt <- plt + geom_ribbon(aes(group = interaction(rib_band, truck_perf)), alpha = 0.2)
plt <- plt + facet_wrap(~ truck_perf)
plt <- plt + theme_bw()
plt