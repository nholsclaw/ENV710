library(dplyr)
library(tidyr)
library(factoextra)

# order and categorize trophic levels
fish_clean$trophic_cat <- cut(
  fish_clean$trophic_level,
  breaks = c(-Inf, 2.9, 3.9, 4.4, Inf),
  labels = c("Primary Consumer", "Secondary Consumer", "Tertiary Consumer", "Apex Predator"),
  right = TRUE
)

# filter out rows with missing data
fish_pca_data <- fish_clean %>%
  select(trophic_cat, FO, Region, Neritic_Oceanic, Bentho_Pelagic) %>%
  drop_na()

# make sure two categ. columns are factors
fish_pca_dummy <- fish_pca_data %>%
  mutate(across(c(Neritic_Oceanic, Bentho_Pelagic), as.factor)) %>%
  model.matrix(~ . - Region - 1, data = .) %>%
  as.data.frame()

# scale the numeric variables
fish_pca_scaled <- scale(fish_pca_dummy)

# run PCA
pca_result <- prcomp(fish_pca_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)

# look through variable contributions
loadings <- pca_result$rotation
head(loadings)


# plot PCA individuals with points colored by region
trophic_pca <- fviz_pca_ind(pca_result,
             geom.ind = "point",
             col.ind = fish_pca_data$trophic_cat, # color by __
             palette = "Dark2",
             addEllipses = TRUE, 
             legend.title = "Trophic Category") +
  xlab("PC1 (18% variance explained)") +
  ylab("PC2 (17.3% variance explained)") +
  theme_minimal()
trophic_pca

# plot PCA variables
var_pca <- fviz_pca_var(pca_result,
             col.var = "black",
             repel = TRUE) +
  xlab("PC1 (18% variance explained)") +
  ylab("PC2 (17.3% variance explained)") +
  theme_minimal()
var_pca

# save plots
ggsave(here("figures/trophic_pca.png"),
       plot = trophic_pca, width = 6, height = 5, dpi = 300)
ggsave(here("figures/var_pca.png"),
       plot = var_pca, width = 6, height = 5, dpi = 300)

combined_pca <- trophic_pca | var_pca
combined_pca
ggsave(here("figures/combined_pca.png"),
       plot = combined_pca, width = 11, height = 9, dpi = 300)


