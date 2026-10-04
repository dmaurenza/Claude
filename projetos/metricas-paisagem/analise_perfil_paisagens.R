# Landscape profile along the forest cover gradient ----
# Input: ./Results/lsm.csv (one row per site and class; output of metricas_paisagem.R)

library(tidyverse)

dir_output <- "./Results"
dir.create(file.path(dir_output, "Figures"), showWarnings = FALSE, recursive = TRUE)

## Step 1 - Loading data ----

lsm <- read_csv("./Results/lsm.csv")
glimpse(lsm)

# Class names (reclassification of metricas_paisagem.R; check against the MapBiomas legend)
class_names <- tibble(
  class = c(1, 3, 4, 2, 5, 0),
  class_name = c("Forest", "Farming", "Non-vegetated", "Non-forest natural", "Water", "Not observed")
) %>%
  mutate(class_name = factor(class_name, levels = class_name)) # stack order of the figures

class_colors <- c("Forest" = "#008300", "Farming" = "#eda100", "Non-vegetated" = "#4a3aa7",
                  "Non-forest natural" = "#eb6834", "Water" = "#2a78d6", "Not observed" = "#b5b3ad")

## Step 2 - Site table: one row per site and class (absent class = 0%) ----

sites <- lsm %>%
  distinct(id_unique, datasetId, siteId, yearStart)

# forest cover of each site = pland of class 1
forest <- lsm %>%
  filter(class == 1) %>%
  select(id_unique, forest_cover = pland)

composition <- lsm %>%
  select(id_unique, class, pland, ed, np) %>%
  complete(id_unique, class = class_names$class, fill = list(pland = 0, ed = 0, np = 0)) %>%
  left_join(class_names, by = "class") %>%
  left_join(forest, by = "id_unique") %>%
  left_join(sites, by = "id_unique")

# check: pland of each site sums to 100
composition %>%
  group_by(id_unique) %>%
  summarise(total = sum(pland)) %>%
  summary()

# forest cover classes (10% bins)
composition <- composition %>%
  mutate(forest_bin = cut(forest_cover, breaks = seq(0, 100, 10), include.lowest = TRUE,
                          labels = paste0(seq(0, 90, 10), "-", seq(10, 100, 10))))

## Step 3 - Summary tables ----

# number of sites in each forest cover class
n_sites <- composition %>%
  distinct(id_unique, forest_bin) %>%
  count(forest_bin, name = "n_sites")
n_sites

# mean composition (pland) of each class along the gradient
profile_table <- composition %>%
  group_by(forest_bin, class_name) %>%
  summarise(mean_pland = mean(pland), .groups = "drop") %>%
  pivot_wider(names_from = class_name, values_from = mean_pland) %>%
  left_join(n_sites, by = "forest_bin") %>%
  relocate(n_sites, .after = forest_bin)
profile_table

# how often each class is present in the landscape (% of sites)
presence_table <- composition %>%
  group_by(forest_bin, class_name) %>%
  summarise(present = round(100 * mean(pland > 0), 1), .groups = "drop") %>%
  pivot_wider(names_from = class_name, values_from = present)
presence_table

write_csv(profile_table, file.path(dir_output, "profile_mean_pland.csv"))
write_csv(presence_table, file.path(dir_output, "profile_presence.csv"))

## Step 4 - Figures ----

theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  legend.position = "bottom",
                  legend.title = element_blank()))

### Fig 1 - Distribution of sites along the forest cover gradient ----
fig1 <- composition %>%
  distinct(id_unique, forest_cover) %>%
  ggplot(aes(x = forest_cover)) +
  geom_histogram(breaks = seq(0, 100, 5), fill = "#008300", color = "white", linewidth = 0.5) +
  labs(x = "Forest cover in the 2 km buffer (%)", y = "Number of sites",
       title = "Sites along the forest cover gradient")
fig1

### Fig 2 - Mean landscape composition in each forest cover class ----
fig2 <- composition %>%
  group_by(forest_bin, class_name) %>%
  summarise(mean_pland = mean(pland), .groups = "drop") %>%
  ggplot(aes(x = forest_bin, y = mean_pland, fill = class_name)) +
  geom_col(width = 0.8, color = "white", linewidth = 0.4,
           position = position_stack(reverse = TRUE)) +
  geom_text(data = n_sites, aes(x = forest_bin, y = 103, label = paste0("n=", n_sites)),
            inherit.aes = FALSE, size = 3, color = "grey30") +
  scale_fill_manual(values = class_colors) +
  scale_y_continuous(breaks = seq(0, 100, 25), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Forest cover class (%)", y = "Mean landscape composition (%)",
       title = "What surrounds the forest along the gradient")
fig2

### Fig 3 - Each land use class along the continuous gradient ----
fig3 <- composition %>%
  filter(class_name != "Forest") %>%
  ggplot(aes(x = forest_cover, y = pland)) +
  geom_point(aes(color = class_name), alpha = 0.25, size = 1, show.legend = FALSE) +
  geom_smooth(method = "loess", se = TRUE, color = "grey20", linewidth = 0.8) +
  scale_color_manual(values = class_colors) +
  facet_wrap(~ class_name, scales = "free_y") + # each class with its own y scale
  labs(x = "Forest cover (%)", y = "Class cover (%)",
       title = "Land use classes along the forest cover gradient")
fig3

### Fig 4 - Forest configuration along the gradient (edge density and number of patches) ----
fig4 <- composition %>%
  filter(class_name == "Forest") %>%
  select(id_unique, forest_cover, `Edge density (m/ha)` = ed, `Number of forest patches` = np) %>%
  pivot_longer(-c(id_unique, forest_cover)) %>%
  ggplot(aes(x = forest_cover, y = value)) +
  geom_point(color = "#008300", alpha = 0.25, size = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "grey20", linewidth = 0.8) +
  facet_wrap(~ name, scales = "free_y") +
  labs(x = "Forest cover (%)", y = NULL, title = "Forest fragmentation along the gradient")
fig4

## Step 5 - Saving figures ----

ggsave(file.path(dir_output, "Figures", "fig1_forest_gradient.png"), fig1, width = 7, height = 4.5, dpi = 300, bg = "white")
ggsave(file.path(dir_output, "Figures", "fig2_composition_profile.png"), fig2, width = 9, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(dir_output, "Figures", "fig3_classes_gradient.png"), fig3, width = 9, height = 6, dpi = 300, bg = "white")
ggsave(file.path(dir_output, "Figures", "fig4_forest_configuration.png"), fig4, width = 9, height = 4.5, dpi = 300, bg = "white")
