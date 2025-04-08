library(readxl)
library(ggplot2)
library(dplyr)
library(here)


fish_data2 <- read.csv(here("cleaned_fish_data.csv"))

# need to fill in gaps in data set because they left duplicate values / names as a blank
fish_data_FINAL <- fish_data2 %>%
  fill(species, common_name, Neritic_Oceanic, Bentho_Pelagic, feeding_strategy,
     trophic_level, IUCN, fisheries_interest, G.GI, method, N, n_fish_ingested, FO, Region, .direction = "down")


write.csv(fish_data_FINAL, here("fish_data_FINAL.csv"))
testing <- read.csv(here("fish_data_FINAL.csv"))




fish_data$trophic_level <- as.numeric(as.character(fish_data$trophic_level))
fish_data$FO <- as.numeric(as.character(fish_data$FO))

