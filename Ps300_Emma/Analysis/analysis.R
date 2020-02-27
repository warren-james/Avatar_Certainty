library(tidyverse)

files <- dir("../Data")
files <- paste("../Data/", files[str_which(files, "enddat")], sep = "")

 map_df(files[str_which(files, "enddat")], read_csv) %>%
 	mutate(
 		participant = factor(rep(1:36, each = 12)),
 		nsp = abs(Placed_x / Delta),
 		precision = as_factor(Spread),
 		precision = fct_recode(precision, random = "1", uncertain = "10", certain = "1000")) -> d 



ggplot(d, aes(x = Delta, y = nsp, colour = participant)) + geom_point(alpha = 0.5) +
	geom_smooth(method = "lm", aes(group = 1)) + facet_wrap(~ precision) +
	ggthemes::theme_tufte() + theme(text=element_text(size=16,  family="Comic Sans MS"))
	ggsave("firetrucks_ps300_2020.png", width = 8, height = 5)


library(lme4)

m1 <- lmer(data = d, nsp ~ Delta * precision + (1|participant))	
m2 <- lmer(data = d, nsp ~ Delta + (1|participant))	