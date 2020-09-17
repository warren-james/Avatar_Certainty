library(tidyverse)

files <- dir("../Data")
files <- paste("../Data/", files[str_which(files, "enddat")], sep = "")

 map_df(files[str_which(files, "enddat")], read_csv) %>%
 	mutate(
 		participant = factor(rep(1:36, each = 12)),
 		position_normalised = abs(Placed_x / Delta),
 		precision = as_factor(Spread),
 		success = Success,
 		precision = fct_recode(precision, random = "1", uncertain = "10", certain = "1000")) -> d 



ggplot(d, aes(x = Delta, y = position_normalised, colour = participant)) + geom_point(alpha = 0.5) +
	geom_smooth(method = "lm", aes(group = 1)) + facet_wrap(~ precision) +
	ggthemes::theme_tufte() + theme(text=element_text(size=16,  family="Comic Sans MS"))
	ggsave("firetrucks_ps300_2020.png", width = 8, height = 5)





library(lme4)

m1 <- lmer(data = d, position_normalised ~ Delta * precision + (1|participant))	
m2 <- lmer(data = d, position_normalised ~ Delta + (1|participant))	


d %>% 
	select(participant, precision, Delta, success, position_normalised) %>% 
	mutate(
		distance =as.factor(Delta),
		distance = fct_recode(distance, close = "200", close = "420", far = "640", far = "860")) %>%
	group_by(participant, precision, distance) %>%
	summarise(
		mean_position = mean(position_normalised),
		accuracy = mean(success)) -> d2

write_csv(d2, "Emma_firetruck_data.csv")

ggplot(d2, aes(x = distance, fill = precision, y = mean_position)) + geom_boxplot()