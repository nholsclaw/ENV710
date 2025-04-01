library(readxl)
library(ggplot2)
library(dplyr)
library(here)


fish_data <- read_excel(here("fish_data_all.xlsx"), sheet = "trophic_level")

fish_data$trophic_level <- as.numeric(as.character(fish_data$trophic_level))
fish_data$FO <- as.numeric(as.character(fish_data$FO))

# scatterplot
ggplot(fish_data, aes(x = trophic_level, y = FO, color = as.factor(trophic_category))) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.6, size = 2) +  # Adds jitter to separate overlapping points
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", size = 1) +  # Dashed trendline
  labs(x = "Trophic Level", y = "Frequency of Plastic Ingestion (FO)",
       title = "Relationship Between Trophic Level and Plastic Ingestion",
       color = "Trophic Category") +
  scale_color_brewer(palette = "Dark2") +  # Uses a better color scheme
  theme_classic() +
  theme(legend.position = "top")  # Moves legend to the top


fish_summary <- fish_data %>%
  group_by(trophic_level) %>%
  summarise(mean_FO = mean(FO, na.rm = TRUE))

ggplot(fish_data, aes(x = trophic_level, y = FO)) +
  geom_jitter(width = 0.1, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", size = 1) +
  geom_point(data = fish_summary, aes(x = trophic_level, y = mean_FO), 
             color = "red", size = 4, shape = 18) +  # Adds red mean points
  labs(x = "Trophic Level", y = "Plastic Ingestion (FO)",
       title = "Relationship Between Trophic Level and Plastic Ingestion") +
  theme_classic()

fish_summary <- fish_data %>%
  group_by(trophic_level) %>%
  summarise(mean_FO = mean(FO, na.rm = TRUE))

ggplot(fish_summary, aes(x = trophic_level, y = mean_FO)) +
  geom_point(color = "red", size = 4, shape = 16) +  # Mean points
  geom_smooth(method = "lm", se = TRUE, color = "blue", size = 1) +  # Regression on means
  labs(x = "Trophic Level", y = "Mean Plastic Ingestion (FO)",
       title = "Mean Plastic Ingestion Across Trophic Levels") +
  theme_classic()



# boxplots

fish_data <- fish_data %>%
  mutate(trophic_category = case_when(
    trophic_level < 2 ~ "Primary Consumers",
    trophic_level >= 2 & trophic_level < 3 ~ "Secondary Consumers",
    trophic_level >= 3 & trophic_level < 4 ~ "Tertiary Consumers",
    trophic_level >= 4 ~ "Top Predators"
  ))

ggplot(fish_data, aes(x = as.factor(trophic_category), y = FO, fill = trophic_category)) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +  # Raw data points
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +  # Boxplot without outliers
  labs(x = "Trophic Level", 
       y = "Plastic Ingestion Frequency (FO)", 
       title = "Plastic Ingestion Across Trophic Levels") +
  theme_classic()

# Remove NAs from FO before plotting
fish_data_filtered <- fish_data %>%
  filter(!is.na(trophic_category))

# Density plot without NAs
ggplot(fish_data_filtered, aes(x = FO, fill = as.factor(trophic_category))) +
  geom_density(alpha = 0.4) +
  labs(x = "Plastic Ingestion (FO)", y = "Density",
       title = "Distribution of Plastic Ingestion Across Trophic Levels") +
  theme_classic()


