# Landscape metrics in wide format: one row per site ----
# Input: ./Results/lsm.csv (one row per site and class)
# Output columns: ed and np of class 1 (forest), pland of classes 1 to 5 named by class

library(tidyverse)

lsm <- read_csv("./Results/lsm.csv")

# Class names (reclassification of metricas_paisagem.R)
class_names <- tibble(
  class = c(1, 3, 4, 2, 5),
  class_name = c("Forest", "Farming", "Non vegetated", "Herbaceous or Shrubby Vegetation", "Water")
) %>%
  mutate(class_name = factor(class_name, levels = class_name)) # stack order of the figures

# CHANGED: column name of each pland: "area" + class name without spaces
# (e.g. "Forest" -> areaForest, "Non vegetated" -> areaNonVegetated)
class_names <- class_names %>%
  mutate(pland_col = paste0("area", str_remove_all(str_to_title(class_name), "[^A-Za-z]")),
         pland_col = factor(pland_col, levels = pland_col)) # same order as class_name

# ed and np of class 1 (forest), with the site identifiers
class1 <- lsm %>%
  filter(class == 1) %>%
  select(id_unique, datasetId, siteId, yearStart, ed_c1 = ed, np_c1 = np)

# pland of each class; a class absent from the buffer has no row, so it gets 0%
pland_wide <- lsm %>%
  select(id_unique, class, pland) %>%
  complete(id_unique, class = class_names$class, fill = list(pland = 0)) %>%
  left_join(class_names, by = "class") %>%
  select(id_unique, pland_col, pland) %>%
  pivot_wider(names_from = pland_col, values_from = pland, names_sort = TRUE)

lsm_wide <- class1 %>%
  left_join(pland_wide, by = "id_unique")

# checks: one row per site, and pland of each site sums to 100
stopifnot(!any(duplicated(lsm_wide$id_unique)))
summary(rowSums(select(lsm_wide, all_of(levels(class_names$pland_col)))))

glimpse(lsm_wide)

write_csv(lsm_wide, "./Results/lsm_wide.csv")
