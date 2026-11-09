# Landscape metrics in wide format: one row per site ----
# Input: ./Results/lsm.csv (one row per site and class)
# Output columns: ed and np of class 1 (forest), pland of classes 1 to 5

library(tidyverse)

lsm <- read_csv("./Results/lsm.csv")

# ed and np of class 1 (forest), with the site identifiers
class1 <- lsm %>%
  filter(class == 1) %>%
  select(id_unique, datasetId, siteId, yearStart, ed_c1 = ed, np_c1 = np)

# pland of classes 1 to 5; a class absent from the buffer has no row, so it gets 0%
pland_wide <- lsm %>%
  select(id_unique, class, pland) %>%
  complete(id_unique, class = 1:5, fill = list(pland = 0)) %>%
  pivot_wider(names_from = class, values_from = pland,
              names_prefix = "pland_c", names_sort = TRUE)

lsm_wide <- class1 %>%
  left_join(pland_wide, by = "id_unique") %>%
  select(id_unique, datasetId, siteId, yearStart,
         ed_c1, np_c1, pland_c1, pland_c2, pland_c3, pland_c4, pland_c5)

# checks: one row per site, and pland of each site sums to 100
stopifnot(!any(duplicated(lsm_wide$id_unique)))
summary(rowSums(select(lsm_wide, starts_with("pland"))))

glimpse(lsm_wide)

write_csv(lsm_wide, "./Results/lsm_wide.csv")
