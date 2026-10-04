library(sf)
library(tidyverse)
library(rnaturalearth)
library(ggspatial) # install.packages("ggspatial")
library(geobr)
library(terra)

# Input files ----

# Atlantic Forest limit (original extent)
af <- sf::read_sf("Data/SHP/limites_integradores_wgs84_v1_2_0/ma_limite_integrador_muylaert_et_al_2018_wgs84_v1_1_0.shp")

# CHANGED: current forest remnants (your layer)
# - raster (.tif): set forest_values to the pixel values that mean forest
#   (e.g. 1 for a forest / non-forest map; c(3, 4, 5, 6, 49) for MapBiomas collection codes)
# - vector (.shp or .gpkg): every polygon is drawn as forest
forest_now_path <- "Data/forest_remnants.tif"
forest_values <- 1
forest_now_year <- 2022 # only used in the legend

# Sampling sites
database_gpkg <- read_sf("Results/full_database.gpkg")

# Countries and Brazilian states
sa <- rnaturalearth::ne_countries(continent = "South America", returnclass = "sf")
sa[sa$name_en == "Brazil", "name_en"] <- ""
sa_label <- st_point_on_surface(sa)

states <- geobr::read_state(code_state = "all", year = 2010, simplified = T) %>%
  st_transform(crs = st_crs(sa))

# CHANGED: other biomes removed

af <- st_transform(af, crs = st_crs(sa))
af_v <- vect(af)

# Map extent and shared theme ----

map_xlim <- c(-60, -32)
map_ylim <- c(-35, 0)

map_theme <- theme(
  panel.background = element_rect(fill = "white"),
  plot.background = element_rect(fill = "white", color = NA),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  axis.ticks.length = unit(0.15, "cm"),
  axis.text = element_text(size = 9, color = "black"),
  panel.border = element_rect(fill = NA, color = "black", linewidth = 0.6),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10, face = "bold"),
  strip.background = element_rect(fill = "gray80", color = "black", linewidth = 0.6),
  strip.text = element_text(size = 10)
)

# CHANGED: boundaries are lines, so their legend keys are line segments (key_glyph = "path")
boundary_colors <- c("Atlantic Forest limit" = "gray20",
                     "Country boundaries" = "gray50",
                     "State boundaries" = "gray75")

boundary_layers <- function(af_fill = NA){
  list(
    geom_sf(data = sa, aes(color = "Country boundaries"), fill = NA,
            linewidth = 0.3, key_glyph = "path"),
    geom_sf(data = states, aes(color = "State boundaries"), fill = NA,
            linewidth = 0.25, key_glyph = "path"),
    geom_sf(data = af, aes(color = "Atlantic Forest limit"), fill = af_fill,
            linewidth = 0.35, key_glyph = "path"),
    geom_sf_text(data = states, aes(label = abbrev_state), size = 2.5,
                 color = "black", check_overlap = TRUE),
    geom_sf_text(data = sa_label, aes(geometry = geometry, label = name_en),
                 size = 2.5, inherit.aes = FALSE),
    scale_color_manual(name = NULL, values = boundary_colors, breaks = names(boundary_colors)),
    coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE)
  )
}

# Figure 1 - Original and current Atlantic Forest ----

# CHANGED: reads the current forest layer (raster or vector)
read_forest_now <- function(path, af_v, forest_values, max_cells = 1e6){

  if(grepl("\\.tiff?$", path, ignore.case = TRUE)){
    r <- rast(path)
    r <- crop(r, project(af_v, crs(r)))
    r <- r %in% forest_values # TRUE = forest

    # coarser grid for mapping (a 30 m raster is too large to draw);
    # each new cell is forest when most of it was forest
    fact <- ceiling(sqrt(ncell(r) / max_cells))
    if(fact > 1) r <- aggregate(r, fact = fact, fun = "mean", na.rm = TRUE)

    r <- project(r, crs(af_v), method = "near") %>%
      mask(af_v)

    df <- as.data.frame(r, xy = TRUE)
    names(df)[3] <- "forest"
    df <- df %>% filter(forest >= 0.5)

    list(type = "raster", data = df)

  } else {
    v <- read_sf(path) %>%
      st_transform(crs = st_crs(af_v)) %>%
      st_intersection(st_union(st_as_sf(af_v)))

    list(type = "vector", data = v)
  }
}

forest_now <- read_forest_now(forest_now_path, af_v, forest_values)

forest_labels <- c("Original extent", paste0("Current forest remnants (", forest_now_year, ")"))
forest_colors <- setNames(c("#d9ead3", "#1b7837"), forest_labels)

if(forest_now$type == "raster"){
  forest_layer <- geom_tile(data = forest_now$data,
                            aes(x = x, y = y, fill = forest_labels[2]))
} else {
  forest_layer <- geom_sf(data = forest_now$data,
                          aes(fill = forest_labels[2]), color = NA)
}

fig1 <- ggplot() +
  geom_sf(data = af, aes(fill = forest_labels[1]), color = NA) +
  forest_layer +
  boundary_layers() +
  scale_fill_manual(name = "Atlantic Forest", values = forest_colors, breaks = forest_labels) +
  scale_x_continuous(breaks = seq(-60, -32, by = 15)) +
  scale_y_continuous(breaks = seq(-30, 0, by = 10)) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering(),
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  annotation_scale(location = "bl", width_hint = 0.2, height = unit(0.15, "cm")) +
  guides(fill = guide_legend(order = 1), color = guide_legend(order = 2)) +
  map_theme +
  theme(legend.position = c(0.98, 0.02), # legend inside the map, bottom right
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = "white", color = NA))
fig1

ggsave("Fig/Fig1_AtlanticForest.png", fig1, width = 7, height = 8, dpi = 600)

# Figure 2 - Kernel density of sampling sites (all groups + each group) ----

# one point per site and taxonomic group
sites_group <- database_gpkg %>%
  distinct(taxonGroup, datasetId, siteId, .keep_all = TRUE) %>%
  select(taxonGroup, datasetId, siteId)

# CHANGED: in "All groups", a site sampled for several groups counts once
sites_all <- sites_group %>%
  distinct(datasetId, siteId, .keep_all = TRUE)

# Kernel density inside the Atlantic Forest limit
# res: 0.045 degrees (~5 km); sigma: 0.225 degrees (~25 km)
kernel_density <- function(points, af_v, res = 0.045, sigma = 0.225){
  r <- rast(af_v, resolution = res)

  p_raster <- rasterize(vect(points), r, fun = "count", background = 0) %>%
    mask(af_v)

  w <- focalMat(p_raster, sigma, type = "Gauss")

  kde <- focal(p_raster, w = w, fun = "sum", na.rm = TRUE) %>%
    mask(af_v)

  df <- as.data.frame(kde, xy = TRUE)
  names(df)[3] <- "Density"
  df
}

# panel names with the number of sites
groups <- sort(unique(sites_group$taxonGroup))
panel_names <- c(paste0("All groups (n = ", nrow(sites_all), ")"),
                 paste0(groups, " (n = ", table(sites_group$taxonGroup)[groups], ")"))

kde_df <- bind_rows(
  kernel_density(sites_all, af_v) %>% mutate(panel = panel_names[1]),
  map2_dfr(groups, panel_names[-1], function(g, p){
    kernel_density(filter(sites_group, taxonGroup == g), af_v) %>% mutate(panel = p)
  })
) %>%
  mutate(panel = factor(panel, levels = panel_names))

last_panel <- data.frame(panel = factor(tail(panel_names, 1), levels = panel_names))

# CHANGED: one-hue sequential palette that starts light (no white, no grey overlay);
# cube-root scale to show low densities, legend in original units
cube_root <- scales::trans_new("cube_root", function(x) x^(1/3), function(x) x^3)

fig2 <- ggplot() +
  geom_tile(data = kde_df, aes(x = x, y = y, fill = Density)) +
  boundary_layers() + # CHANGED: Atlantic Forest limit drawn as an outline only
  scale_fill_gradientn(name = "Kernel density\n(sites, cube-root scale)",
                       colors = c("#fff7bc", "#fee391", "#fec44f", "#fe9929", "#d95f0e", "#993404"),
                       trans = cube_root,
                       breaks = function(l) signif(l[2] * c(0, 1/27, 8/27, 1), 1), # evenly spaced on the cube-root scale
                       na.value = "transparent") +
  scale_x_continuous(breaks = seq(-60, -32, by = 15)) +
  scale_y_continuous(breaks = seq(-30, 0, by = 10)) +
  annotation_north_arrow(data = last_panel, location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering(),
                         height = unit(0.8, "cm"), width = unit(0.8, "cm")) +
  annotation_scale(data = last_panel, location = "bl", width_hint = 0.3,
                   height = unit(0.15, "cm")) +
  facet_wrap(vars(panel), ncol = 3) +
  guides(fill = guide_colorbar(order = 1), color = guide_legend(order = 2)) +
  map_theme +
  theme(legend.position = "right")
fig2

ggsave("Fig/Fig2_Kernel_groups.png", fig2, width = 11, height = 9, dpi = 600)


rm(list = ls())
