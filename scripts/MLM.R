library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(ggeffects)

# filter out any missing data
fish_clean <- fish_data_FINAL %>%
  filter(!is.na(FO), !is.na(trophic_level), !is.na(Region)) %>%
  mutate(
    Region = as.factor(Region),
    Neritic_Oceanic = as.factor(Neritic_Oceanic),
    Bentho_Pelagic = as.factor(Bentho_Pelagic),
    feeding_strategy = as.factor(feeding_strategy)
  )

# convert to numeric
fish_clean$trophic_level <- as.numeric(fish_clean$trophic_level)

# fit the model
mod1 <- lmer(
  FO ~ trophic_level + Neritic_Oceanic + Bentho_Pelagic + feeding_strategy + (1 | Region),
  data = fish_clean
)

summary(mod1)
check_model(mod1)

plot(ggpredict(mod1, terms = c("trophic_level", "Region")))


##### Model 2
# going to log-transform FO because residuals show heteroscedasticity and mild non-normality
# using log(1+x) because FO has 0s 

# going to remove feeding_strategy because it had a high VIF>10, showing collinearity

fish_clean$log_FO <- log1p(fish_clean$FO)

mod2 <- lmer(log_FO ~ trophic_level + Neritic_Oceanic + Bentho_Pelagic + (1 | Region), data = fish_clean)
summary(mod2)

check_model(mod2)

plot(ggpredict(mod2, terms = c("trophic_level", "Region")))
# all regions on top of each other so maybe facet

pred <- ggpredict(mod2, terms = c("trophic_level", "Region"))

mlm2_plot <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  facet_wrap(~group) +
  labs(x = "Trophic Level", y = "Predicted log(FO)", title = "Predicted FO by Trophic Level and Region") +
  theme_minimal()

# save plot
ggsave(here("figures/MLM_plot_regions.png"),
       plot = mlm2_plot, width =8, height = 6, dpi = 300)






